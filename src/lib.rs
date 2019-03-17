/*!
This crate provides `GettersByType` and `GettersMutByType` derive macros for structs, which implements a getter method for each type they contain. The getter methods return an array containing references to all the fields of the same type. The `GettersMutByType` derive also adds a mut version for those methods.

Example using `GettersByType` :

```rust
use getters_by_type::GettersByType;
#[derive(GettersByType)]
struct Test {
    first: i32,
    second: i32,
    third: i32,
}

let test = Test { first: 6, second: 12, third: 24 };

// Let's sum all the i32 fields with a fold expression:
assert_eq!(test.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
```

Example using `GettersMutByType` :


```rust
use getters_by_type::GettersMutByType;

#[derive(Default)]
struct Updater {}
impl Updater {
    fn update(&mut self) {/*...*/}
}

#[derive(GettersMutByType, Default)]
struct Test {
    first: Updater,
    second: Updater,
    /*...*/
onehundredth: Updater,
}

let mut test = Test::default();

// Let's update all the Updater fields
for updater in test.get_mut_fields_updater().iter_mut() {
updater.update();
}
```
!*/

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::*;
use quote::quote;
use std::collections::HashMap;
use syn::DeriveInput;

#[proc_macro_derive(GettersByType)]
pub fn getters_by_type(input: TokenStream) -> TokenStream {
    getters_by_type_impl(input, false)
}

#[proc_macro_derive(GettersMutByType)]
pub fn getters_mut_by_type(input: TokenStream) -> TokenStream {
    getters_by_type_impl(input, true)
}

fn getters_by_type_impl(input: TokenStream, with_mutability: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let (vis, ty, generics) = (&ast.vis, &ast.ident, &ast.generics);
    let fields_by_type = match ast.data {
        syn::Data::Struct(e) => read_fields(e.fields, with_mutability),
        _ => panic!("{} can only be derived for structs.", if with_mutability { "GettersMutByType" } else { "GettersByType" }),
    };
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let methods = fields_by_type.into_iter().fold(Vec::<TokenTree>::new(), |mut acc, (type_name, fields_sharing_type)| {
        let ctx = MethodContext {
            method_return_type: fields_sharing_type.type_ident,
            type_name: fix_type_name(&type_name),
            vis,
        };
        acc.extend(make_method_tokens("get_fields", &ctx, false, fields_sharing_type.immutable_fields));
        if with_mutability {
            acc.extend(make_method_tokens("get_mut_fields", &ctx, true, fields_sharing_type.mutable_fields));
        }
        acc
    });

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

struct MethodContext<'a> {
    method_return_type: syn::Type,
    vis: &'a syn::Visibility,
    type_name: String,
}

fn make_method_tokens(method_prefix: &str, ctx: &MethodContext, mutability: bool, mut field_names: Vec<String>) -> proc_macro2::TokenStream {
    let count = field_names.len();
    let field_idents = field_names.iter_mut().map(|i| syn::Ident::new(&i, proc_macro2::Span::call_site()));
    let method_name = syn::Ident::new(&format!("{}_{}", method_prefix, ctx.type_name), proc_macro2::Span::call_site());
    let (vis, method_return_type) = (&ctx.vis, &ctx.method_return_type);
    if mutability {
        quote! {
            #vis fn #method_name(&mut self) -> [&mut #method_return_type; #count] {
                [#(&mut self.#field_idents),*]
            }
        }
    } else {
        quote! {
            #vis fn #method_name(&self) -> [&#method_return_type; #count] {
                [#(&self.#field_idents),*]
            }
        }
    }
}

struct FieldsSharingType {
    immutable_fields: Vec<String>,
    mutable_fields: Vec<String>,
    type_ident: syn::Type,
}

impl FieldsSharingType {
    fn new(type_ident: syn::Type) -> FieldsSharingType {
        FieldsSharingType {
            immutable_fields: vec![],
            mutable_fields: vec![],
            type_ident,
        }
    }
}

fn read_fields(fields: syn::Fields, with_mutability: bool) -> HashMap<String, FieldsSharingType> {
    let mut fields_by_type = HashMap::<String, FieldsSharingType>::new();
    for field in fields.iter() {
        if let Some(ref ident) = field.ident {
            match get_data_from_field(&field) {
                Ok(FieldInfo { is_mutable, type_ident, type_name }) => {
                    let fields_by_type = fields_by_type.entry(type_name).or_insert_with(|| FieldsSharingType::new(type_ident));
                    if is_mutable && with_mutability {
                        fields_by_type.mutable_fields.push(ident.to_string());
                    }
                    fields_by_type.immutable_fields.push(ident.to_string());
                }
                Err(err) => {
                    eprintln!("[WARNING::GetterByType] {} for field: {}", err, ident);
                }
            }
        }
    }
    fields_by_type
}

struct FieldInfo {
    is_mutable: bool,
    type_ident: syn::Type,
    type_name: String,
}

fn get_data_from_field(field: &syn::Field) -> Result<FieldInfo, &'static str> {
    let (type_name, type_ident, is_field_mutable) = match field.ty {
        syn::Type::Path(ref path) => (get_type_string(path), field.ty.clone(), true),
        syn::Type::Reference(ref reference) => match *reference.elem {
            syn::Type::Path(ref path) => (get_type_string(path), syn::Type::Path(path.clone()), reference.mutability.is_some()),
            _ => return Err("Reference not covered"),
        },
        _ => return Err("Type not covered"),
    };
    Ok(FieldInfo {
        is_mutable: is_field_mutable,
        type_ident,
        type_name: type_name?,
    })
}

fn get_type_string(path: &syn::TypePath) -> Result<String, &'static str> {
    let mut error = None;
    let operation = path
        .path
        .segments
        .iter()
        .map(|segment| {
            segment.ident.to_string()
                + match get_argument_string(&segment.arguments) {
                    Ok(ref string) => string,
                    Err(err) => {
                        error = Some(err);
                        ""
                    }
                }
        })
        .collect::<String>();
    match error {
        None => Ok(operation),
        Some(err) => Err(err),
    }
}

fn get_argument_string(arguments: &syn::PathArguments) -> Result<String, &'static str> {
    let mut type_name = String::new();
    match arguments {
        syn::PathArguments::AngleBracketed(ref angle) => {
            type_name += "<";
            for arg in &angle.args {
                type_name += &match arg {
                    syn::GenericArgument::Type(ref ty) => get_type_path_string(ty),
                    _ => Ok("".into()), /*
                                        This _ should math these four, and they shouldn't be cover by this function:

                                        syn::GenericArgument::Lifetime(_) => {},
                                        syn::GenericArgument::Binding(_) => {},
                                        syn::GenericArgument::Constraint(_) => {},
                                        syn::GenericArgument::Const(_) => {},
                                        */
                }?;
            }
            type_name += ">";
        }
        syn::PathArguments::None => {}
        syn::PathArguments::Parenthesized(ref paren) => {
            type_name += "(";
            for arg in &paren.inputs {
                get_type_path_string(arg)?;
            }
            type_name += ")";
            match paren.output {
                syn::ReturnType::Default => {}
                syn::ReturnType::Type(_, ref arg) => {
                    type_name += "arrow_";
                    get_type_path_string(&**arg)?;
                }
            }
        }
    }
    Ok(type_name)
}

fn get_type_path_string(type_argument: &syn::Type) -> Result<String, &'static str> {
    match type_argument {
        syn::Type::Path(ref argpath) => {
            let mut error = None;
            let operation = argpath
                .path
                .segments
                .iter()
                .map(|argsegment| {
                    format!("{}_", argsegment.ident)
                        + match get_argument_string(&argsegment.arguments) {
                            Ok(ref string) => string,
                            Err(err) => {
                                error = Some(err);
                                ""
                            }
                        }
                })
                .collect::<String>();
            match error {
                None => Ok(operation),
                Some(err) => Err(err),
            }
        }
        _ => Err("Type argument not covered"),
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
