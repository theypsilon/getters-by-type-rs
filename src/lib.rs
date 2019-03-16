/*!
This crate provides `GettersByType` and `GettersMutByType` derive macros for structs, which implements a getter method for each type they contain. The getter methods return an array containing references to all the fields of the same type. The `GettersMutByType` derive also adds a mut version for those methods.

Example using `GettersByType` :

```rust
#[derive(GettersByType)]
struct Test {
    first: i32,
    second: i32,
    third: i32,
}

let test = Test { first: 6, second: 12, third: 24 };

// Let's sum all the i32 field with a fold expression:
assert_eq!(test.get_fields_i32().iter().fold(0, |acc, x| **x + acc), 42);
```

Example using `GettersMutByType` :


```rust
#[derive(GettersMutByType)]
struct Test {
    first: Updater,
    second: Updater,
    ...
    onehundredth: Updater,
}

impl Updater {
    fn update(&mut self) {...}
}

let mut test = Test::new();

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
pub fn fields_getters_mutable_by_type(input: TokenStream) -> TokenStream {
    fields_getters_by_type_impl(input, false)
}

#[proc_macro_derive(GettersMutByType)]
pub fn fields_getters_immutable_by_type(input: TokenStream) -> TokenStream {
    fields_getters_by_type_impl(input, true)
}

struct FieldByTypeValue {
    immutable_fields: Vec<String>,
    mutable_fields: Vec<String>,
    ident: syn::Type,
}

impl FieldByTypeValue {
    fn new(ident: syn::Type) -> FieldByTypeValue {
        FieldByTypeValue {
            immutable_fields: vec![],
            mutable_fields: vec![],
            ident,
        }
    }
}

fn fields_getters_by_type_impl(input: TokenStream, with_mutability: bool) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let (vis, ty, generics) = (&ast.vis, &ast.ident, &ast.generics);
    let mut field_by_type = HashMap::<String, FieldByTypeValue>::new();
    match ast.data {
        syn::Data::Struct(e) => {
            for field in e.fields.iter() {
                let mut type_name = String::new();
                let mut is_field_mutable: bool = true;
                match field.ty {
                    syn::Type::Path(ref path) => {
                        add_type_string(&mut type_name, path);
                        field_by_type
                            .entry(type_name.clone())
                            .or_insert_with(|| FieldByTypeValue::new(field.ty.clone()));
                    }
                    syn::Type::Reference(ref reference) => {
                        match *reference.elem {
                            syn::Type::Path(ref path) => {
                                add_type_string(&mut type_name, path);
                                field_by_type.entry(type_name.clone()).or_insert_with(|| {
                                    FieldByTypeValue::new(syn::Type::Path(path.clone()))
                                });
                            }
                            _ => println!("Reference not covered."),
                        }
                        is_field_mutable = reference.mutability.is_some();
                    }
                    _ => {
                        println!("Type not covered.");
                    }
                }
                match field.ident.as_ref() {
                    Some(ident) => {
                        if let Some(field_info) = field_by_type.get_mut(&type_name) {
                            if is_field_mutable && with_mutability {
                                field_info.mutable_fields.push(ident.to_string());
                            }
                            field_info.immutable_fields.push(ident.to_string());
                        }
                    }
                    None => {
                        println!("Ident is missing.");
                    }
                }
            }
        }
        _ => panic!("GettersByType can only be derived for structs."),
    };
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut ts = Vec::<TokenTree>::new();
    for (current_type, field_info) in field_by_type.into_iter() {
        let method_return_type = field_info.ident.clone();
        let mut add_method = |method_prefix: &str, mutability: bool, mut type_vec: Vec<String>| {
            let count = type_vec.len();
            let subs_vec = type_vec
                .iter_mut()
                .map(|i| syn::Ident::new(&i, proc_macro2::Span::call_site()));
            let method_name = syn::Ident::new(
                &format!("{}_{}", method_prefix, fix_ident(&current_type)),
                proc_macro2::Span::call_site(),
            );
            let method = if mutability {
                quote! {
                    #vis fn #method_name(&mut self) -> [&mut #method_return_type; #count] {
                        [#(&mut self.#subs_vec),*]
                    }
                }
            } else {
                quote! {
                    #vis fn #method_name(&self) -> [&#method_return_type; #count] {
                        [#(&self.#subs_vec),*]
                    }
                }
            };
            ts.extend(method);
        };
        add_method("get_fields", false, field_info.immutable_fields);
        if with_mutability {
            add_method("get_mut_fields", true, field_info.mutable_fields);
        }
    }

    let tokens = quote! {
        impl #impl_generics #ty #ty_generics
            #where_clause
        {
            #(#ts)
            *
        }
    };
    tokens.into()
}

fn add_type_string(type_name: &mut String, path: &syn::TypePath) {
    for segment in &path.path.segments {
        *type_name += &segment.ident.to_string();
        add_argument_string(type_name, &segment.arguments);
    }
}

// KEOPS PYRAMID!!
fn add_argument_string(type_name: &mut String, arguments: &syn::PathArguments) {
    match arguments {
        syn::PathArguments::AngleBracketed(ref angle) => {
            *type_name += "<";
            for arg in &angle.args {
                match arg {
                    syn::GenericArgument::Type(ref ty) => match ty {
                        syn::Type::Path(ref argpath) => {
                            for argsegment in &argpath.path.segments {
                                *type_name += &format!("{}_", argsegment.ident);
                                add_argument_string(type_name, &argsegment.arguments);
                            }
                        }
                        _ => println!("Type argument not covered."),
                    },
                    _ => println!("Generic argument not covered."),
                }
            }
            *type_name += ">";
        }
        syn::PathArguments::None => {}
        _ => println!("PathArguments not covered."),
    }
}

fn fix_ident(name: &str) -> String {
    name.to_string()
        .to_lowercase()
        .chars()
        .map(|c| match c {
            '<' | '>' => '_',
            _ => c,
        })
        .collect()
}
