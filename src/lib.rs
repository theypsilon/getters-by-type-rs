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
            _ => panic!("{} can only be derived for structs.", self.derive_name),
        };
        let mut methods = Vec::<TokenTree>::new();
        for (type_name, fields_sharing_type) in fields_by_type.into_iter() {
            let return_type = MethodReturnType {
                ident: fields_sharing_type.type_ident,
                name: fix_type_name(&type_name),
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

    fn read_fields<'a>(&self, fields: &'a syn::Fields) -> HashMap<String, FieldsSharingType<'a>> {
        let mut fields_by_type = HashMap::<String, FieldsSharingType>::new();
        for field in fields.iter() {
            if let Some(ref ident) = field.ident {
                let info = get_info_from_type(&field.ty);
                match get_type_name_from_type(&field.ty) {
                    Ok(type_name) => {
                        let fields_by_type = fields_by_type.entry(type_name).or_insert_with(|| FieldsSharingType::new(info.type_ident));
                        if info.is_mutable && self.with_mutability {
                            fields_by_type.mutable_fields.push(ident);
                        }
                        fields_by_type.immutable_fields.push(ident);
                    }
                    Err(err) => {
                        eprintln!("[WARNING::{}] {} for field: {}", self.derive_name, err, ident);
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

fn get_type_name_from_type(ty: &syn::Type) -> Result<String, &'static str> {
    let mut type_name = String::with_capacity(64);
    fill_type_name_from_type(&mut type_name, ty)?;
    Ok(type_name)
}

fn fill_type_name_from_type(type_name: &mut String, ty: &syn::Type) -> Result<(), &'static str> {
    match ty {
        syn::Type::Path(ref path) => fill_type_name_from_type_path(type_name, path),
        syn::Type::Reference(ref reference) => match *reference.elem {
            syn::Type::Path(ref path) => fill_type_name_from_type_path(type_name, path),
            _ => Err("Reference not covered"),
        },
        syn::Type::BareFn(ref function) => {
            *type_name += "fn";
            if !function.inputs.is_empty() {
                for arg in function.inputs.iter() {
                    *type_name += "_";
                    fill_type_name_from_type(type_name, &arg.ty)?;
                }
            }
            *type_name += "_";
            fill_type_name_from_return_type(type_name, &function.output)?;
            Ok(())
        }
        syn::Type::Slice(_) => Err("Type Slice not covered"),
        syn::Type::Array(_) => Err("Type Array not covered"),
        syn::Type::Ptr(_) => Err("Type Ptr not covered"),
        syn::Type::Never(_) => Err("Type Never not covered"),
        syn::Type::Tuple(_) => Err("Type Tuple not covered"),
        syn::Type::TraitObject(_) => Err("Type TraitObject not covered"),
        syn::Type::ImplTrait(_) => Err("Type ImplTrait not covered"),
        syn::Type::Paren(_) => Err("Type Paren not covered"),
        syn::Type::Group(_) => Err("Type Group not covered"),
        syn::Type::Infer(_) => Err("Type Infer not covered"),
        syn::Type::Macro(_) => Err("Type Macro not covered"),
        syn::Type::Verbatim(_) => Err("Type Verbatim not covered"),
    }
}

fn fill_type_name_from_type_path(type_name: &mut String, path: &syn::TypePath) -> Result<(), &'static str> {
    for segment in path.path.segments.iter() {
        *type_name += &segment.ident.to_string();
        fill_type_name_from_path_arguments(type_name, &segment.arguments)?;
    }
    Ok(())
}

fn fill_type_name_from_path_arguments(type_name: &mut String, arguments: &syn::PathArguments) -> Result<(), &'static str> {
    match arguments {
        syn::PathArguments::AngleBracketed(ref angle) => {
            *type_name += "<";
            for arg in &angle.args {
                match arg {
                    syn::GenericArgument::Type(ref ty) => {
                        fill_type_name_from_type(type_name, ty)?;
                        *type_name += "_";
                    }
                    syn::GenericArgument::Lifetime(_) => {}
                    syn::GenericArgument::Binding(_) => {}
                    syn::GenericArgument::Constraint(_) => {}
                    syn::GenericArgument::Const(_) => {}
                };
            }
            type_name.truncate(type_name.len() - 1);
            *type_name += ">";
        }
        syn::PathArguments::None => {}
        syn::PathArguments::Parenthesized(ref paren) => {
            *type_name += "(";
            for arg in &paren.inputs {
                fill_type_name_from_type(type_name, arg)?;
                *type_name += "_";
            }
            type_name.truncate(type_name.len() - 1);
            *type_name += ")";
            fill_type_name_from_return_type(type_name, &paren.output)?;
        }
    }
    Ok(())
}

fn fill_type_name_from_return_type(type_name: &mut String, output: &syn::ReturnType) -> Result<(), &'static str> {
    match output {
        syn::ReturnType::Default => Ok(()),
        syn::ReturnType::Type(_, ref arg) => fill_type_name_from_type(type_name, &**arg),
    }
}

fn fix_type_name(name: &str) -> String {
    name.to_string()
        .to_lowercase()
        .chars()
        .map(|c| match c {
            '<' | '>' | '(' | ')' | '-' => '_',
            _ => c,
        })
        .collect()
}
