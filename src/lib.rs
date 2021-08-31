//! This crate allows the user to represent state in the database using Rust enums. This is achieved
//! through a proc macro. First the macro looks at your chosen `sql_type`, and then it devises a
//! corresponding Rust type. The mapping is as follows:
//! 
//! | SQL | Rust |
//! |--|--|
//! | `SmallInt` | `i16` |
//! | `Integer` | `i32` |
//! | `Int` | `i32` |
//! | `BigInt` | `i64` |
//! | `VarChar` | `String` |
//! | `Text` | `String` |
//! 
//!  The macro then generates three impls: a `FromSql` impl, an `ToSql` impl and a
//! `TryFrom` impl, which allow conversion between the Sql type an the enum (`FromSql` and `ToSql`),
//! and from the Rust type into the enum (`TryInto`).
//!
//! ### Usage
//! ```rust
//! #[macro_use] extern crate diesel;
//! use diesel_enum::DbEnum;
//! use diesel::sql_types::SmallInt;
//! 
//! #[derive(Debug)]
//! pub struct CustomError {
//!     msg: String,
//!     status: u16,
//! }
//! 
//! impl CustomError {
//!     fn not_found(msg: String) -> Self {
//!         Self {
//!             msg,
//!             status: 404,
//!         }
//!     }
//! }
//! 
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
//! #[sql_type = "SmallInt"]
//! #[error_fn = "CustomError::not_found"]
//! #[error_type = "CustomError"]
//! pub enum Status {
//!     /// Will be represented as 0.
//!     Ready,
//!     /// Will be represented as 1.
//!     Pending,
//! }
//! ```
//! Alternatively you can use strings, with will be cast to lowercase. (e.g. `Status::Ready` will be
//! stored as `"ready"` in the database):
//! ```rust
//! #[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
//! #[sql_type = "VarChar"]
//! #[error_fn = "CustomError::not_found"]
//! #[error_type = "CustomError"]
//! pub enum Status {
//!     /// Will be represented as `"ready"`.
//!     Ready,
//!     /// Will be represented as `"pending"`.
//!     Pending,
//! }
//! ```

use quote::quote;

struct MacroState<'a> {
    name: syn::Ident,
    variants: Vec<&'a syn::Variant>,
    sql_type: syn::Ident,
    rust_type: syn::Ident,
    error_type: syn::Path,
    error_fn: syn::Path,
}

impl<'a> MacroState<'a> {
    fn repr(variant: &syn::Variant) -> Option<syn::Lit> {
        let repr = variant
            .attrs
            .iter()
            .find(|a| a.path.get_ident().map(|i| i == "val").unwrap_or(false))
            .map(|a| a.tokens.to_string())?;
        let trimmed = repr[1..].trim();
        Some(syn::parse_str(&trimmed).unwrap())
    }

    fn rust_type(sql_type: &str) -> syn::Ident {
        let name = match sql_type.to_string().as_str() {
            "SmallInt" => "i16",
            "Integer" | "Int" => "i32",
            "BigInt" => "i64",
            "VarChar" | "Text" => "String",
            otherwise => panic!("Invalid `sql_type`: {}", otherwise),
        };
        let span = proc_macro2::Span::call_site();
        syn::Ident::new(name, span)
    }

    fn try_from(&self) -> proc_macro2::TokenStream {
        let span = proc_macro2::Span::call_site();
        let variants = self.variants.iter().map(|f| &f.ident);
        let error_fn = &self.error_fn;
        let name = self.name.to_string();
        let conversion = match self.rust_type.to_string().as_str() {
            "i16" | "i32" | "i64" => {
                let nums = self
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(idx, &var)| (syn::LitInt::new(&idx.to_string(), span), var))
                    .map(|(idx, var)| (syn::Lit::Int(idx), var))
                    .map(|(idx, var)| Self::repr(var).unwrap_or(idx));
                quote! {
                    match inp {
                        #(#nums => Ok(Self::#variants),)*
                        otherwise => {
                            Err(#error_fn(format!("Unexpected `{}`: {}", #name, otherwise)))
                        },
                    }
                }
            },
            "String" => {
                let field_names = self
                    .variants
                    .iter()
                    .map(|v| {
                        use syn::{Lit::Str, LitStr};
                        let fallback = v.ident.to_string().to_lowercase();
                        Self::repr(v).unwrap_or(Str(LitStr::new(&fallback, span)))
                    });

                quote! {
                    match inp.as_str() {
                        #(#field_names => Ok(Self::#variants),)*
                        otherwise => {
                            Err(#error_fn(format!("Unexpected `{}`: {}", #name, otherwise)))
                        },
                    }
                }
            },
            _ => panic!(),
        };

        let error_type = &self.error_type;
        let rust_type = &self.rust_type;
        let name = &self.name;
        quote! {
            impl TryFrom<#rust_type> for #name {
                type Error = #error_type;

                fn try_from(inp: #rust_type) -> Result<Self, Self::Error> {
                    #conversion
                }
            }
        }
    }

    fn into_impl(&self) -> proc_macro2::TokenStream {
        let span = proc_macro2::Span::call_site();
        let rust_type = &self.rust_type;
        let name = &self.name;
        let variants = self.variants.iter().map(|f| &f.ident);
        let conversion = match self.rust_type.to_string().as_str() {
            "i16" | "i32" | "i64" => {
                let nums = self
                    .variants
                    .iter()
                    .enumerate()
                    .map(|(idx, &var)| (syn::LitInt::new(&idx.to_string(), span), var))
                    .map(|(idx, var)| (syn::Lit::Int(idx), var))
                    .map(|(idx, var)| Self::repr(var).unwrap_or(idx));
                quote! {
                    match self {
                        #(Self::#variants => #nums as #rust_type,)*
                    }
                }
            },
            "String" => {
                let field_names = self
                    .variants
                    .iter()
                    .map(|v| {
                        use syn::{Lit::Str, LitStr};
                        let fallback = v.ident.to_string().to_lowercase();
                        Self::repr(v).unwrap_or(Str(LitStr::new(&fallback, span)))
                    });

                quote! {
                    match self {
                        #(Self::#variants => #field_names,)*
                    }
                }
            },
            _ => panic!(),
        };

        quote! {
            impl Into<#rust_type> for #name {
                fn into(self) -> #rust_type {
                    #conversion.into()
                }
            }
        }
    }

    fn from_sql(&self) -> proc_macro2::TokenStream {
        let sql_type = &self.sql_type;
        let rust_type = &self.rust_type;
        let name = &self.name;
        
        quote! {
            impl<Db> FromSql<#sql_type, Db> for #name
            where
                Db: diesel::backend::Backend,
                #rust_type: FromSql<#sql_type, Db>
            {
                fn from_sql(bytes: Option<&Db::RawValue>) -> deserialize::Result<Self> {
                    let s = <#rust_type as FromSql<#sql_type, Db>>::from_sql(bytes)?;
                    Ok(s.try_into().unwrap())
                }
            }
        }
    }

    fn to_sql(&self) -> proc_macro2::TokenStream {
        let span = proc_macro2::Span::call_site();
        let sql_type = &self.sql_type;
        let rust_type = &self.rust_type;
        let name = &self.name;
        let conversion = match self.rust_type.to_string().as_str() {
            "i16" | "i32" | "i64" => quote! {
                let i: #rust_type = (*self).into();
                ToSql::<#sql_type, Db>::to_sql(&i, out)
            },
            "String" => {
                let variants = self.variants.iter().map(|f| &f.ident);
                let field_names = self
                    .variants
                    .iter()
                    .map(|&v| {
                        use syn::{Lit::Str, LitStr};
                        let fallback = v.ident.to_string().to_lowercase();
                        Self::repr(v).unwrap_or(Str(LitStr::new(&fallback, span)))
                    });

                quote! {
                    let s = match self {
                        #(Self::#variants => #field_names,)*
                    };
                    ToSql::<#sql_type, Db>::to_sql(s, out)
                }
            },
            _ => panic!(),
        };

        quote! {
            impl<Db: diesel::backend::Backend> ToSql<#sql_type, Db> for #name {
                fn to_sql<W: Write>(&self, out: &mut Output<'_, W, Db>) -> serialize::Result {
                    #conversion
                }
            }
        }
    }
}

fn get_attr_ident<'a>(attrs: &'a [syn::Attribute], name: &str) -> syn::Ident {
    let stream = attrs
        .iter()
        .find(|a| a.path.get_ident().map(|i| i == name).unwrap_or(false))
        .map(|a| &a.tokens)
        .expect(&format!("Need `{}`", name))
        .to_string();
    let trimmed = trim_attr(&stream);
    let span = proc_macro2::Span::call_site();
    syn::Ident::new(trimmed, span)
}

fn get_attr_path<'a>(attrs: &'a [syn::Attribute], name: &str) -> syn::Path {
    let stream = attrs
        .iter()
        .find(|a| a.path.get_ident().map(|i| i == name).unwrap_or(false))
        .map(|a| &a.tokens)
        .expect(&format!("Need `{}`", name))
        .to_string();
    let trimmed = trim_attr(&stream);
    syn::parse_str(trimmed).unwrap()
}

fn trim_attr(attr: &str) -> &str {
    let quoted = attr.trim()[1..].trim();
    &quoted[1..quoted.len() - 1]
}

#[proc_macro_derive(DbEnum, attributes(sql_type, error_fn, error_type, val))]
pub fn db_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident;
    let sql_type = get_attr_ident(&input.attrs, "sql_type");
    let error_fn = get_attr_path(&input.attrs, "error_fn");
    let error_type = get_attr_path(&input.attrs, "error_type");
    let rust_type = MacroState::rust_type(&sql_type.to_string());
    let data = match input.data {
        syn::Data::Enum(data) => data,
        _ => panic!("needs enum"),
    };
    let variants = data.variants.iter().collect();
    let state = MacroState {
        name,
        variants,
        sql_type,
        rust_type,
        error_fn,
        error_type
    };
    let from_sql = state.from_sql();
    let to_sql = state.to_sql();
    let try_from = state.try_from();
    let into = state.into_impl();
    let name = state.name;
    let sql_type = state.sql_type;
    let error_type = state.error_type;
    let error_mod = state.error_fn.segments.first().expect("need `error_fn`");
    let error_type_str = error_type
        .segments
        .iter()
        .fold(String::new(), |a, b| a + &b.ident.to_string() + "::");
    let error_type_str = &error_type_str[..error_type_str.len() - 2];
    let error_import = if error_mod.ident.to_string() == error_type_str {
        quote! {}
    } else {
        quote! { use super::#error_mod; }
    };

    (quote! {
        #[allow(non_snake_case, unused_extern_crates, unused_imports)]
        mod __impl_db_enum {
            use super::{#name, #error_type};
            #error_import

            use diesel::{
                self,
                deserialize::{self, FromSql},
                serialize::{self, Output, ToSql},
                sql_types::#sql_type,
            };
            use std::{
                convert::{TryFrom, TryInto},
                io::Write,
            };

            #[automatically_derived]
            #from_sql
            
            #[automatically_derived]
            #to_sql
            
            #[automatically_derived]
            #try_from

            #[automatically_derived]
            #into
        }
    }).into()
}
