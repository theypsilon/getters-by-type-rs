//!
//! This crate provides `GettersByType` and `GettersMutByType` derive macros for structs, which implements a getter method for each type they contain.
//! The getter methods return an array containing references to all the fields of the same type.
//! The `GettersMutByType` derive also adds a mut version for those methods.
//!
//! Example using `GettersByType` :
//!
//! ```rust
//! use getters_by_type::GettersByType;
//! #[derive(GettersByType)]
//! struct S {
//!     first: i32,
//!     second: i32,
//!     third: i32,
//! }
//!
//! let object = S { first: 6, second: 12, third: 24 };
//!
//! // Let's sum all the i32 fields with a fold expression:
//! assert_eq!(object.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
//! ```
//!
//! For more documentation and examples, see the [GettersByType derive](derive.GettersByType.html) documentation.
//!
//! Example using `GettersMutByType` :
//!
//!
//! ```rust
//! use getters_by_type::GettersMutByType;
//!
//! #[derive(Default)]
//! struct Updater {}
//! impl Updater {
//!     fn update(&mut self) {/*...*/}
//! }
//!
//! #[derive(GettersMutByType, Default)]
//! struct S {
//!     first: Updater,
//!     second: Updater,
//!     /*...*/
//! onehundredth: Updater,
//! }
//!
//! let mut object = S::default();
//!
//! // Let's update all the Updater fields
//! for updater in object.get_mut_fields_updater().iter_mut() {
//! updater.update();
//! }
//! ```
//!
//! For more documentation and examples, see the [GettersMutByType derive](derive.GettersMutByType.html) documentation.

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::quote;
use std::collections::HashMap;

/// The `GettersByType` macro automatically generates an `impl` for the given struct,
/// implementing a getter method for each different type contained within the struct.
///
/// ```rust
/// use getters_by_type::GettersByType;
/// #[derive(GettersByType)]
/// struct S {
///     a: bool,
///     b: bool,
/// }
///
/// // Would generete:
///
/// impl S {
///     fn get_fields_bool(&self) -> [&bool; 2] {
///         [&self.a, &self.b]
///     }
/// }
/// ```
///
/// It works also with generic types in the following way.
/// Types containing < and >,
///
#[proc_macro_derive(GettersByType)]
pub fn getters_by_type(input: TokenStream) -> TokenStream {
    ImplContext::new(input, "GettersByType", false).transform_ast()
}

#[proc_macro_derive(GettersMutByType)]
pub fn getters_mut_by_type(input: TokenStream) -> TokenStream {
    ImplContext::new(input, "GettersByMutType", true).transform_ast()
}

struct ImplContext {
    ast: syn::DeriveInput,
    derive_name: &'static str,
    with_mutability: bool,
}

impl ImplContext {
    fn new(input: TokenStream, derive_name: &'static str, with_mutability: bool) -> ImplContext {
        ImplContext {
            ast: syn::parse(input).expect("Could not parse AST."),
            derive_name,
            with_mutability,
        }
    }

    fn transform_ast(&self) -> TokenStream {
        let fields_by_type = match self.ast.data {
            syn::Data::Struct(ref class) => self.read_fields(&class.fields),
            _ => panic!(
                "The type '{}' is not a struct but tries to derive '{}' which can only be used on structs.",
                self.ast.ident, self.derive_name
            ),
        };
        let mut methods = Vec::<TokenTree>::new();
        for (type_pieces, fields_sharing_type) in fields_by_type.into_iter() {
            let return_type = MethodReturnType {
                ident: fields_sharing_type.type_ident,
                name: make_type_name_from_type_pieces(type_pieces),
            };
            methods.extend(self.make_method_tokens("get_fields", &return_type, false, fields_sharing_type.immutable_fields));
            if self.with_mutability {
                methods.extend(self.make_method_tokens("get_mut_fields", &return_type, true, fields_sharing_type.mutable_fields));
            }
        }
        let (ty, generics) = (&self.ast.ident, &self.ast.generics);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let tokens = quote! {
            impl #impl_generics #ty #ty_generics
                #where_clause
            {
                #(#methods)
                *
            }
        };
        tokens.into()
    }

    fn read_fields<'a>(&self, fields: &'a syn::Fields) -> HashMap<Vec<TypePart<'a>>, FieldsSharingType<'a>> {
        let mut fields_by_type = HashMap::<Vec<TypePart>, FieldsSharingType>::new();
        for field in fields.iter() {
            if let Some(ref ident) = field.ident {
                let info = get_info_from_type(&field.ty);
                match make_idents_from_type(&field.ty) {
                    Ok(type_pieces) => {
                        let fields_by_type = fields_by_type.entry(type_pieces).or_insert_with(|| FieldsSharingType::new(info.type_ident));
                        if info.is_mutable && self.with_mutability {
                            fields_by_type.mutable_fields.push(ident);
                        }
                        fields_by_type.immutable_fields.push(ident);
                    }
                    Err(err) => {
                        eprintln!("[WARNING::{}] Field '{}' of struct '{}' not covered because: {}", self.derive_name, ident, self.ast.ident, err);
                    }
                }
            }
        }
        fields_by_type
    }

    fn make_method_tokens(&self, method_prefix: &str, return_type: &MethodReturnType, mutability: bool, field_idents: Vec<&syn::Ident>) -> proc_macro2::TokenStream {
        let count = field_idents.len();
        let method_name = syn::Ident::new(&format!("{}_{}", method_prefix, return_type.name), Span::call_site());
        let (vis, return_type) = (&self.ast.vis, &return_type.ident);
        if mutability {
            quote! {
                #vis fn #method_name(&mut self) -> [&mut #return_type; #count] {
                    [#(&mut self.#field_idents),*]
                }
            }
        } else {
            quote! {
                #vis fn #method_name(&self) -> [&#return_type; #count] {
                    [#(&self.#field_idents),*]
                }
            }
        }
    }
}

struct MethodReturnType<'a> {
    ident: &'a syn::Type,
    name: String,
}

struct FieldsSharingType<'a> {
    immutable_fields: Vec<&'a syn::Ident>,
    mutable_fields: Vec<&'a syn::Ident>,
    type_ident: &'a syn::Type,
}

impl<'a> FieldsSharingType<'a> {
    fn new(type_ident: &'a syn::Type) -> FieldsSharingType {
        FieldsSharingType {
            immutable_fields: vec![],
            mutable_fields: vec![],
            type_ident,
        }
    }
}

struct TypeInfo<'a> {
    is_mutable: bool,
    type_ident: &'a syn::Type,
}

#[derive(Hash, PartialEq, Eq)]
enum TypePart<'a> {
    Ident(&'a syn::Ident),
    Separator(&'static str),
}

impl<'a> TypePart<'a> {
    fn to_string(&self) -> String {
        match self {
            TypePart::Ident(i) => i.to_string(),
            TypePart::Separator(s) => s.to_string(),
        }
    }
}

fn get_info_from_type(ty: &syn::Type) -> TypeInfo {
    let (type_ident, is_mutable) = match ty {
        syn::Type::Reference(ref reference) => match *reference.elem {
            syn::Type::Path(_) => (&*reference.elem, reference.mutability.is_some()),
            _ => (ty, true),
        },
        _ => (ty, true),
    };
    TypeInfo { is_mutable, type_ident }
}

fn make_idents_from_type<'a>(ty: &'a syn::Type) -> Result<Vec<TypePart<'a>>, &'static str> {
    let mut type_pieces = Vec::<TypePart<'a>>::with_capacity(8);
    fill_type_pieces_from_type(&mut type_pieces, ty)?;
    Ok(type_pieces)
}

fn fill_type_pieces_from_type<'a>(type_pieces: &mut Vec<TypePart<'a>>, ty: &'a syn::Type) -> Result<(), &'static str> {
    match ty {
        syn::Type::Path(ref path) => fill_type_pieces_from_type_path(type_pieces, path),
        syn::Type::Reference(ref reference) => match *reference.elem {
            syn::Type::Path(ref path) => fill_type_pieces_from_type_path(type_pieces, path),
            _ => Err("syn::Type::Reference is partially implemented at the moment."),
        },
        syn::Type::BareFn(ref function) => {
            type_pieces.push(TypePart::Separator("fn"));
            if !function.inputs.is_empty() {
                for arg in function.inputs.iter() {
                    type_pieces.push(TypePart::Separator("_"));
                    fill_type_pieces_from_type(type_pieces, &arg.ty)?;
                }
            }
            type_pieces.push(TypePart::Separator("_"));
            fill_type_pieces_from_return_type(type_pieces, &function.output)?;
            Ok(())
        }
        syn::Type::Slice(_) => Err("syn::Type::Slice are not implemented yet."),
        syn::Type::Array(_) => Err("syn::Type::Array are not implemented yet."),
        syn::Type::Ptr(_) => Err("syn::Type::Ptr are not implemented yet."),
        syn::Type::Never(_) => Err("syn::Type::Never are not implemented yet."),
        syn::Type::Tuple(_) => Err("syn::Type::Tuple are not implemented yet."),
        syn::Type::TraitObject(_) => Err("syn::Type::TraitObject are not implemented yet."),
        syn::Type::ImplTrait(_) => Err("syn::Type::ImplTrait are not implemented yet."),
        syn::Type::Paren(_) => Err("syn::Type::Paren are not implemented yet."),
        syn::Type::Group(_) => Err("syn::Type::Group are not implemented yet."),
        syn::Type::Infer(_) => Err("syn::Type::Infer are not implemented yet."),
        syn::Type::Macro(_) => Err("syn::Type::Macro are not implemented yet."),
        syn::Type::Verbatim(_) => Err("syn::Type::Verbatim are not implemented yet."),
    }
}

fn fill_type_pieces_from_type_path<'a>(type_pieces: &mut Vec<TypePart<'a>>, path: &'a syn::TypePath) -> Result<(), &'static str> {
    for segment in path.path.segments.iter() {
        type_pieces.push(TypePart::Ident(&segment.ident));
        fill_type_pieces_from_path_arguments(type_pieces, &segment.arguments)?;
    }
    Ok(())
}

fn fill_type_pieces_from_path_arguments<'a>(type_pieces: &mut Vec<TypePart<'a>>, arguments: &'a syn::PathArguments) -> Result<(), &'static str> {
    match arguments {
        syn::PathArguments::AngleBracketed(ref angle) => {
            type_pieces.push(TypePart::Separator("<"));
            for arg in &angle.args {
                match arg {
                    syn::GenericArgument::Type(ref ty) => {
                        fill_type_pieces_from_type(type_pieces, ty)?;
                        type_pieces.push(TypePart::Separator(","));
                    }
                    syn::GenericArgument::Lifetime(_) => {}
                    syn::GenericArgument::Binding(_) => {}
                    syn::GenericArgument::Constraint(_) => {}
                    syn::GenericArgument::Const(_) => {}
                };
            }
            type_pieces.truncate(type_pieces.len() - 1);
            type_pieces.push(TypePart::Separator(">"));
        }
        syn::PathArguments::None => {}
        syn::PathArguments::Parenthesized(ref paren) => {
            type_pieces.push(TypePart::Separator("("));
            for arg in &paren.inputs {
                fill_type_pieces_from_type(type_pieces, arg)?;
                type_pieces.push(TypePart::Separator(","));
            }
            type_pieces.truncate(type_pieces.len() - 1);
            type_pieces.push(TypePart::Separator(")"));
            fill_type_pieces_from_return_type(type_pieces, &paren.output)?;
        }
    }
    Ok(())
}

fn fill_type_pieces_from_return_type<'a>(type_pieces: &mut Vec<TypePart<'a>>, output: &'a syn::ReturnType) -> Result<(), &'static str> {
    match output {
        syn::ReturnType::Default => Ok(()),
        syn::ReturnType::Type(_, ref arg) => fill_type_pieces_from_type(type_pieces, &**arg),
    }
}

fn make_type_name_from_type_pieces(type_pieces: Vec<TypePart>) -> String {
    type_pieces
        .into_iter()
        .map(|piece| piece.to_string())
        .collect::<String>()
        .to_lowercase()
        .chars()
        .map(|c| match c {
            '<' | '>' | '(' | ')' | '-' | ',' => '_',
            _ => c,
        })
        .collect()
}
