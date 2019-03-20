//!
//! This crate provides [`GettersByType`](derive.GettersByType.html) derive macro for structs, which implements a getter method for each type they contain.
//!
//! The generated methods start with the prefix `get_fields_` and end with a transcription of the type they refer.
//! 
//! Each method returns an array with references of all the fields having the type that method refer to.
//!
//! Example using `GettersByType` :
//!
//! ```rust
//! use getters_by_type::GettersByType;
//! #[derive(GettersByType)]
//! struct Foo {
//!     first: i32,
//!     second: i32,
//!     third: i32,
//! }
//!
//! let object = Foo { first: 6, second: 12, third: 24 };
//!
//! // Let's sum all the i32 fields with a fold expression:
//! assert_eq!(object.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
//! ```
//!
//! As you notice, the getter methods return an array containing references to all the fields of the same type.
//! In that example, the return type of the method `get_fields_i32` would be `[&i32; 3]`.
//!
//! This crate also provides a `mut` version [`GettersMutByType`](derive.GettersMutByType.html) which also adds a mut version for those methods.
//!
//! In this case, the generated methods start with the prefix `get_mut_fields_` instead.
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
//! struct Foo {
//!     first: Updater,
//!     second: Updater,
//!     /*...*/
//!     onehundredth: Updater,
//! }
//!
//! let mut object = Foo::default();
//!
//! // Let's update all the Updater fields
//! for updater in object.get_mut_fields_updater().iter_mut() {
//!     updater.update();
//! }
//! ```
//!
//! In this example, the return type of the method `get_mut_fields_updater` would be `[&mut Updater; 3]`.
//! There is no dynamic memory allocation happening within the getter methods, as they just return a fixed array with references.
//! There isn't also unsafe code being generated.
//! For more documentation and examples, see each respective documentation section.

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenTree};
use quote::quote;
use std::collections::HashMap;

/// The `GettersByType` macro automatically generates an `impl` for the given struct,
/// implementing a getter method for each different type contained within the struct.
///
/// The generated methods start with the prefix `get_fields_` and end with a transcription of the type they refer.
/// 
/// Each method returns an array with references of all the fields having the type that method refer to.
///
/// Example:
///
/// ```rust
/// use getters_by_type::GettersByType;
/// #[derive(GettersByType)]
/// struct Foo {
///     a: String,
///     b: String,
/// }
///
/// // Would generete:
///
/// impl Foo {
///     fn get_fields_string(&self) -> [&String; 2] {
///         [&self.a, &self.b]
///     }
/// }
/// ```
///
/// As you notice, the chars of all the types (`String` in this case) go
/// to the method signature in lowercase form.
///
/// It works the same with generic, reference and other types, with the following exceptions:
/// 1. Characters `<` `>` `(` `)` `[` `]` `,` `;` always get converted to `_`.
/// 2. Return type arrow `->` and reference character `&` get ignored completely.
/// 3. Pointer types `*const` and `*mut` get converted o `ptr_const_` and `ptr_mut_` respectively.
///
/// Also, reference types and non-reference types will be covered by the same
/// method, as the methods are always returning references in the first place.
///
/// Example for fn, generic and reference types (more examples for other types later):
///
/// ```rust
/// use getters_by_type::GettersByType;
/// #[derive(GettersByType)]
/// struct Foo {
///     a: Result<i32, i32>,
///     b: Result<i32, Result<i32, i32>>,
///     c: fn(usize) -> f32,
///     d: &'static bool,
///     e: bool,
/// }
///
/// // Would generate:
///
/// impl Foo {
///     fn get_fields_result_i32_i32_(&self) -> [&Result<i32, i32>; 1] {
///         [&self.a]
///     }
///     fn get_fields_result_i32_result_i32_i32__(&self) -> [&Result<i32, Result<i32, i32>>; 1] {
///         [&self.b]
///     }
///     fn get_fields_fn_usize___f32(&self) -> [&fn(usize) -> f32; 1] {
///         [&self.c]
///     }
///     fn get_fields_bool(&self) -> [&bool; 2] {
///         [&self.d, &self.e]
///     }
/// }
/// ```
///
/// Other examples of types to method signature conversions here:
///
/// ```rust
/// use getters_by_type::GettersByType;
/// #[derive(GettersByType)]
/// struct Foo<'a> {
///     a: &'a str,
///     b: Box<Fn(i32) -> f32>,
///     c: &'a Vec<&'a Option<&'a i32>>,
///     d: Result<i32, Result<i32, Result<i32, &'static str>>>,
///     e: Option<Option<Option<Option<Option<fn(usize)>>>>>,
///     f: (i32, i32),
///     g: [i32; 2],
///     h: &'a [&'a Option<&'a i32>],
///     i: *const i32,
///     j: *mut i32,
///     k: Box<dyn Bar>,
/// }
/// trait Bar {}
/// impl Bar for i32 {}
/// let vector = vec!();
/// let number_1 = 1;
/// let mut number_2 = 2;
/// let o = Foo {
///     a: "",
///     b: Box::new(|_| 0.0),
///     c: &vector,
///     d: Ok(0),
///     e: None,
///     f: (0, 0),
///     g: [0, 0],
///     h: vector.as_slice(),
///     i: &number_1,
///     j: &mut number_2,
///     k: Box::new(0),
/// };
/// // from type: &'a str
/// o.get_fields_str();
/// // from type: Box<Fn(i32) -> f32>
/// o.get_fields_box_fn_i32_f32_();
/// // from type: &'a Vec<&'a Option<&'a i32>>
/// o.get_fields_vec_option_i32__();
/// // from type: Result<i32, Result<i32, Result<i32, &'static str>>>
/// o.get_fields_result_i32_result_i32_result_i32_str___();
/// // from type: Option<Option<Option<Option<Option<fn(usize)>>>>>
/// o.get_fields_option_option_option_option_option_fn_usize______();
/// // from type: (i32, i32)
/// o.get_fields__i32_i32_();
/// // from type: [i32; 2]
/// o.get_fields__i32_2_();
/// // from type: &'a [&'a Option<&'a i32>]
/// o.get_fields__option_i32__();
/// // from type: *const i32
/// o.get_fields_ptr_const_i32();
/// // from type: *mut i32
/// o.get_fields_ptr_mut_i32();
/// // from type: Box<dyn Bar>
/// o.get_fields_box_dyn_bar_();
/// ```
///
/// Method visibility is inherited directly from the struct visibility,
/// so if the struct is public, all the methods generated by `GettersByType`
/// will be public too. There is no fine-grained control for fields visibility.
///
/// There are still some types not implemented. Those are the following:
///
/// * `TraitObject` is partially implemented, `Box<dyn Trait>` works, but `&dyn Trait` doesn't.
/// * `Never`
/// * `ImplTrait`
/// * `Group`
/// * `Infer`
/// * `Macro`
/// * `Verbatim`
///
/// Hopefully, they will get implemented in next releases.
///
#[proc_macro_derive(GettersByType)]
pub fn getters_by_type(input: TokenStream) -> TokenStream {
    ImplContext::new(input, "GettersByType", false).transform_ast()
}

/// The `GettersMutByType` macro automatically generates an `impl` for the given struct,
/// implementing a getter method for each different type contained within the struct.
///
/// The generated methods start with the prefix `get_mut_fields_` and end with a transcription of the type they refer.
///
/// Example:
///
/// ```rust
/// use getters_by_type::GettersMutByType;
/// #[derive(GettersMutByType)]
/// struct Foo {
///     a: String,
///     b: String,
/// }
///
/// // Would generete:
///
/// impl Foo {
///     fn get_mut_fields_string(&mut self) -> [&mut String; 2] {
///         [&mut self.a, &mut self.b]
///     }
/// }
/// ```
///
/// This is the mutable version of `GettersByType`.
/// The same rules are applying, so check the [GettersByType derive](derive.GettersByType.html) documentation first.
///
/// There is one important difference, thought. There are some fields with types that
/// can't be made mutable. I.e. types with immutable references. When that's the case,
/// the field gets ignored completely.
///
/// Example:
///
/// ```rust
/// use getters_by_type::GettersMutByType;
/// #[derive(GettersMutByType)]
/// struct Foo<'a> {
///     a: &'a String,
///     b: String,
/// }
///
/// let string = String::new();
/// let mut o = Foo {
///     a: &string,
///     b: "".into(),
/// };
///
/// assert_eq!(o.get_mut_fields_string().len(), 1); // instead of 2
/// ```
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
                ty: fields_sharing_type.ty,
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
                        let fields_by_type = fields_by_type.entry(type_pieces).or_insert_with(|| FieldsSharingType::new(info.ty));
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
        let (vis, return_type) = (&self.ast.vis, &return_type.ty);
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
    ty: &'a syn::Type,
    name: String,
}

struct FieldsSharingType<'a> {
    immutable_fields: Vec<&'a syn::Ident>,
    mutable_fields: Vec<&'a syn::Ident>,
    ty: &'a syn::Type,
}

impl<'a> FieldsSharingType<'a> {
    fn new(ty: &'a syn::Type) -> FieldsSharingType {
        FieldsSharingType {
            immutable_fields: vec![],
            mutable_fields: vec![],
            ty,
        }
    }
}

struct TypeInfo<'a> {
    is_mutable: bool,
    ty: &'a syn::Type,
}

#[derive(Hash, PartialEq, Eq)]
enum TypePart<'a> {
    Ident(&'a syn::Ident),
    Integer(u64),
    Separator(&'static str),
}

impl<'a> TypePart<'a> {
    fn to_string(&self) -> String {
        match self {
            TypePart::Ident(i) => i.to_string(),
            TypePart::Separator(s) => s.to_string(),
            TypePart::Integer(i) => i.to_string(),
        }
    }
}

fn get_info_from_type(ty: &syn::Type) -> TypeInfo {
    let (ty, is_mutable) = match ty {
        syn::Type::Reference(ref reference) => (&*reference.elem, reference.mutability.is_some()),
        _ => (ty, true),
    };
    TypeInfo { is_mutable, ty }
}

fn make_idents_from_type<'a>(ty: &'a syn::Type) -> Result<Vec<TypePart<'a>>, &'static str> {
    let mut type_pieces = Vec::<TypePart<'a>>::with_capacity(8);
    fill_type_pieces_from_type(&mut type_pieces, ty)?;
    Ok(type_pieces)
}

fn fill_type_pieces_from_type<'a>(type_pieces: &mut Vec<TypePart<'a>>, ty: &'a syn::Type) -> Result<(), &'static str> {
    match ty {
        syn::Type::Path(ref path) => fill_type_pieces_from_type_path(type_pieces, &path.path),
        syn::Type::Reference(ref reference) => fill_type_pieces_from_type(type_pieces, &reference.elem),
        syn::Type::BareFn(ref function) => {
            type_pieces.push(TypePart::Separator("fn("));
            fill_type_pieces_from_array_of_inputs(type_pieces, &function.inputs, ",", |type_pieces, arg| fill_type_pieces_from_type(type_pieces, &arg.ty))?;
            type_pieces.push(TypePart::Separator(")"));
            fill_type_pieces_from_return_type(type_pieces, &function.output)?;
            Ok(())
        }
        syn::Type::Slice(slice) => {
            type_pieces.push(TypePart::Separator("["));
            fill_type_pieces_from_type(type_pieces, &slice.elem)?;
            type_pieces.push(TypePart::Separator("]"));
            Ok(())
        }
        syn::Type::Array(array) => {
            type_pieces.push(TypePart::Separator("["));
            fill_type_pieces_from_type(type_pieces, &array.elem)?;
            type_pieces.push(TypePart::Separator(";"));
            match &array.len {
                syn::Expr::Lit(lit) => match &lit.lit {
                    syn::Lit::Int(int) => type_pieces.push(TypePart::Integer(int.value())),
                    _ => return Err("syn::Lit::* are not implemented yet."),
                },
                _ => return Err("syn::Expr::* are not implemented yet."),
            }
            type_pieces.push(TypePart::Separator("]"));
            Ok(())
        }
        syn::Type::Tuple(tuple) => {
            type_pieces.push(TypePart::Separator("("));
            fill_type_pieces_from_array_of_inputs(type_pieces, &tuple.elems, ",", fill_type_pieces_from_type)?;
            type_pieces.push(TypePart::Separator(")"));
            Ok(())
        }
        syn::Type::Paren(paren) => {
            type_pieces.push(TypePart::Separator("("));
            fill_type_pieces_from_type(type_pieces, &paren.elem)?;
            type_pieces.push(TypePart::Separator(")"));
            Ok(())
        }
        syn::Type::Ptr(ptr) => {
            type_pieces.push(TypePart::Separator("ptr_"));
            if ptr.const_token.is_some() {
                type_pieces.push(TypePart::Separator("const_"));
            }
            if ptr.mutability.is_some() {
                type_pieces.push(TypePart::Separator("mut_"));
            }
            fill_type_pieces_from_type(type_pieces, &ptr.elem)?;
            Ok(())
        }
        syn::Type::ImplTrait(_) => Err("syn::Type::ImplTrait can not be implemented."), // ImplTrait is not valid outside of functions and inherent return types, so can't be implemented.
        syn::Type::TraitObject(trait_object) => {
            if trait_object.dyn_token.is_some() {
                type_pieces.push(TypePart::Separator("dyn_"));
            }
            fill_type_pieces_from_array_of_inputs(type_pieces, &trait_object.bounds, "+", |type_pieces, bound| match bound {
                syn::TypeParamBound::Trait(trait_bound) => fill_type_pieces_from_type_path(type_pieces, &trait_bound.path),
                syn::TypeParamBound::Lifetime(_) => Ok(()),
            })
        }
        syn::Type::Never(_) => Err("syn::Type::Never is not implemented yet."),
        syn::Type::Group(_) => Err("syn::Type::Group is not implemented yet."),
        syn::Type::Infer(_) => Err("syn::Type::Infer is not implemented yet."),
        syn::Type::Macro(_) => Err("syn::Type::Macro is not implemented yet."),
        syn::Type::Verbatim(_) => Err("syn::Type::Verbatim is not implemented yet."),
    }
}

fn fill_type_pieces_from_type_path<'a>(type_pieces: &mut Vec<TypePart<'a>>, path: &'a syn::Path) -> Result<(), &'static str> {
    for segment in path.segments.iter() {
        type_pieces.push(TypePart::Ident(&segment.ident));
        fill_type_pieces_from_path_arguments(type_pieces, &segment.arguments)?;
    }
    Ok(())
}

fn fill_type_pieces_from_path_arguments<'a>(type_pieces: &mut Vec<TypePart<'a>>, arguments: &'a syn::PathArguments) -> Result<(), &'static str> {
    match arguments {
        syn::PathArguments::AngleBracketed(ref angle) => {
            type_pieces.push(TypePart::Separator("<"));
            fill_type_pieces_from_array_of_inputs(type_pieces, &angle.args, ",", |type_pieces, arg| match arg {
                syn::GenericArgument::Type(ref ty) => fill_type_pieces_from_type(type_pieces, ty),
                syn::GenericArgument::Lifetime(_) => Ok(()),
                syn::GenericArgument::Binding(_) => Ok(()),
                syn::GenericArgument::Constraint(_) => Ok(()),
                syn::GenericArgument::Const(_) => Ok(()),
            })?;
            type_pieces.push(TypePart::Separator(">"));
        }
        syn::PathArguments::None => {}
        syn::PathArguments::Parenthesized(ref paren) => {
            type_pieces.push(TypePart::Separator("("));
            fill_type_pieces_from_array_of_inputs(type_pieces, &paren.inputs, ",", fill_type_pieces_from_type)?;
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

fn fill_type_pieces_from_array_of_inputs<'a, T, U>(
    type_pieces: &mut Vec<TypePart<'a>>,
    inputs: &'a syn::punctuated::Punctuated<T, U>,
    separator: &'static str,
    action: impl Fn(&mut Vec<TypePart<'a>>, &'a T) -> Result<(), &'static str>,
) -> Result<(), &'static str> {
    if !inputs.is_empty() {
        for arg in inputs {
            action(type_pieces, arg)?;
            match type_pieces[type_pieces.len() - 1] {
                TypePart::Separator(_s) if _s == separator => {}
                _ => type_pieces.push(TypePart::Separator(separator)),
            }
        }
        match type_pieces[type_pieces.len() - 1] {
            TypePart::Separator(_s) if _s == separator => type_pieces.truncate(type_pieces.len() - 1),
            _ => {}
        }
    }
    Ok(())
}

fn make_type_name_from_type_pieces(type_pieces: Vec<TypePart>) -> String {
    type_pieces
        .into_iter()
        .map(|piece| piece.to_string())
        .collect::<String>()
        .to_lowercase()
        .chars()
        .map(|c| match c {
            '<' | '>' | '(' | ')' | '[' | ']' | '-' | ',' | ';' => '_',
            _ => c,
        })
        .collect()
}
