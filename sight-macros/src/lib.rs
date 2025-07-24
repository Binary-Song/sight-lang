use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::collections::HashMap;
use syn::{self, parse::Parse};

#[proc_macro_derive(Item)]
pub fn derive_item(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let gen = quote! {
        impl crate::container::Item for #name {}
    };
    gen.into()
}

#[proc_macro_derive(LiteralValue)]
pub fn derive_literal_value(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let body = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => {
                let field_names = fields_named.named.iter().map(|f| f.ident.as_ref().unwrap());
                let field_idents: Vec<syn::Ident> = field_names
                    .clone()
                    .map(|ident| syn::Ident::new(&ident.to_string(), ident.span()))
                    .collect();
                let field_pairs = field_names.zip(field_idents.iter()).map(|(name, ident)| {
                    quote! { format!("{}:{}", stringify!(#name), #ident.literal_value()) }
                });
                quote! {
                    let #name { #(#field_idents),* } = self;
                    format!(
                        "{}{{{}}}",
                        stringify!(#name),
                        [#(#field_pairs),*].join(", ")
                    )
                }
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                let field_idents: Vec<syn::Ident> = (0..fields_unnamed.unnamed.len())
                    .map(|i| syn::Ident::new(&format!("f{}", i), name.span()))
                    .collect();
                let field_calls = field_idents.iter().map(|ident| {
                    quote! { #ident.literal_value() }
                });
                quote! {
                    let #name( #(#field_idents),* ) = self;
                    format!(
                        "{}({})",
                        stringify!(#name),
                        [#(#field_calls),*].join(", ")
                    )
                }
            }
            syn::Fields::Unit => {
                quote! {
                    stringify!(#name).to_string()
                }
            }
        },
        syn::Data::Enum(data_enum) => {
            let variant_arms = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    syn::Fields::Named(fields_named) => {
                        let field_names =
                            fields_named.named.iter().map(|f| f.ident.as_ref().unwrap());
                        let field_idents: Vec<syn::Ident> = field_names
                            .clone()
                            .map(|ident| syn::Ident::new(&ident.to_string(), ident.span()))
                            .collect();
                        let field_pairs = field_names.zip(field_idents.iter()).map(|(name, ident)| {
                            quote! { format!("{}:{}", stringify!(#name), #ident.literal_value()) }
                        });
                        quote! {
                            #name::#variant_name { #(#field_idents),* } => format!(
                                "{}::{}{{{}}}",
                                stringify!(#name),
                                stringify!(#variant_name),
                                [#(#field_pairs),*].join(", ")
                            )
                        }
                    }
                    syn::Fields::Unnamed(fields_unnamed) => {
                        let field_idents: Vec<syn::Ident> = (0..fields_unnamed.unnamed.len())
                            .map(|i| syn::Ident::new(&format!("f{}", i), variant_name.span()))
                            .collect();
                        let field_calls = field_idents.iter().map(|ident| {
                            quote! { #ident.literal_value() }
                        });
                        quote! {
                            #name::#variant_name( #(#field_idents),* ) => format!(
                                "{}::{}({})",
                                stringify!(#name),
                                stringify!(#variant_name),
                                [#(#field_calls),*].join(", ")
                            )
                        }
                    }
                    syn::Fields::Unit => {
                        quote! {
                            #name::#variant_name => format!(
                                "{}::{}",
                                stringify!(#name),
                                stringify!(#variant_name)
                            )
                        }
                    }
                }
            });
            quote! {
                match self {
                    #(#variant_arms),*
                }
            }
        }
        _ => todo!(),
    };
    let impl_block = quote! {
        impl crate::LiteralValue for #name {
            fn literal_value(&self) -> String {
                #body
            }
        }
    };
    impl_block.into()
}

#[proc_macro_derive(NumConv)]
pub fn derive_num_conv(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;

    let variants = if let syn::Data::Enum(data_enum) = &ast.data {
        data_enum.variants.iter().collect::<Vec<_>>()
    } else {
        panic!("NumConv can only be derived for enums");
    };

    let try_from_arms = variants.iter().enumerate().map(|(i, v)| {
        let ident = &v.ident;
        quote! { #i => Ok(#name::#ident), }
    });
    let into_arms = variants.iter().enumerate().map(|(i, v)| {
        let ident = &v.ident;
        quote! { #name::#ident => #i   , }
    });

    let gen = quote! {
        impl TryFrom<usize> for #name {
            type Error = ();
            fn try_from(value: usize) -> Result<Self, Self::Error> {
                match value {
                    #(#try_from_arms)*
                    _ => Err(()),
                }
            }
        }
        impl From<#name> for usize {
            fn from(val: #name) -> usize {
                match val {
                    #(#into_arms)*
                }
            }
        }
    };
    gen.into()
}

#[proc_macro_derive(StaticInternable)]
pub fn derive_static_internable(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let gen = quote! {
        impl crate::utils::interning::Internable for #name {}
        impl crate::utils::interning::StaticInternable for #name {
            fn interner() -> &'static std::thread::LocalKey<std::cell::RefCell<crate::utils::interning::Interner<#name>>> {
                use crate::utils::interning::Interner;
                std::thread_local! {
                    static INTERNER: std::cell::RefCell<Interner<#name>> = std::cell::RefCell::new(Interner::<#name>::new());
                }
                &INTERNER
            }
        }
    };
    gen.into()
}

#[proc_macro_derive(Internable)]
pub fn derive_internable(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let gen = quote! {
        impl crate::utils::interning::Internable for #name {}
    };
    gen.into()
}

struct SumIdArgs {
    name: syn::Ident,
    name_id: syn::Ident,
    arms: Vec<(syn::Ident, syn::Type)>,
}

impl Parse for SumIdArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let keyword = input.parse::<syn::Ident>()?;
        assert_eq!(
            keyword.to_string(),
            "target_type",
            "Expected 'sum_type' as the first argument"
        );
        input.parse::<syn::Token![:]>()?;
        let name = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![,]>()?;

        let keyword = input.parse::<syn::Ident>()?;
        assert_eq!(
            keyword.to_string(),
            "id_type",
            "Expected 'sum_type' as the first argument"
        );
        input.parse::<syn::Token![:]>()?;
        let name_id = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![,]>()?;

        let mut arms = vec![];
        while !input.is_empty() {
            // arm example: `foo: i32`
            let arm_name = input.parse::<syn::Ident>()?;
            input.parse::<syn::Token![:]>()?;
            let arm_type = input.parse::<syn::Type>()?;
            arms.push((arm_name, arm_type));
            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
            } else {
                break;
            }
        }
        Ok(SumIdArgs {
            name,
            name_id,
            arms,
        })
    }
}

fn make_sum_id_impl(input: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let args = syn::parse2::<SumIdArgs>(input).expect("Failed to parse SumIdArgs");
    let name = &args.name;
    let arms = &args.arms;
    let name_id = &args.name_id;

    let variants = arms.iter().map(|(arm_name, arm_type)| {
        quote! { #arm_name(#arm_type) }
    });

    let variants_id = arms.iter().map(|(arm_name, arm_type)| {
        quote! { #arm_name(Id<#arm_type>) }
    });

    let converts = arms.iter().map(|(arm_name, arm_type)| {
        quote! {
            impl From<#arm_type> for #name {
                fn from(value: #arm_type) -> Self {
                    #name::#arm_name(value)
                }
            }
        }
    });

    let decode_arms = arms.iter().map(|(arm_name, arm_type)| {
        quote! {
            #name_id::#arm_name(id) => #name::#arm_name(id.decode(container)?)
        }
    });

    let decode_f_arms = arms.iter().map(|(arm_name, arm_type)| {
        quote! {
            #name_id::#arm_name(id) => #name::#arm_name(id.decode_f(container))
        }
    });

    let decode_ex_arms = arms.iter().map(|(arm_name, arm_type)| {
        quote! {
            #name_id::#arm_name(id) => #name::#arm_name(id.decode_ex(container)?)
        }
    });

    quote! {

        #[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
        pub enum #name {
            #(#variants),*
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, LiteralValue)]
        pub enum #name_id {
            #(#variants_id),*
        }

        static_assertions::assert_not_impl_all!(#name: crate::container::Item);

        #(#converts)*

        impl #name_id {
            pub fn decode<C: Container>(self, container: &C) -> Option<#name> {
                Some(match self {
                    #(#decode_arms),*
                })
            }
            pub fn decode_f<C: Container>(self, container: &C) -> #name {
                match self {
                    #(#decode_f_arms),*
                }
            }
            pub fn decode_ex<C: Container>(self, container: &C) -> Result<#name, DecodeError> {
                Ok(match self {
                    #(#decode_ex_arms),*
                })
            }
        }
    }
}

/// Generates a sum type and its corresponding ID type for container-based storage.
///
/// Creates two enums: one holding actual values, another holding `Id<T>` references.
/// Also generates `From` impls and container decode methods. (See unit test for details.)
///
/// Example:
///
/// ```ignore
/// make_sum_id!(
///     target_type: Animal,
///     id_type: AnimalId,
///     Dog: DogStruct,
///     Cat: CatStruct
/// );
/// ```
///
/// **where `Dog` and `Cat` must implement `Item` and 
/// `Animal` must NOT implement `Item`.** 
///
/// Generates `Animal` enum with actual values, `AnimalId` enum with `Id<T>` variants,
/// `From` implementations, and decode methods (`decode`, `decode_f`, `decode_ex`).
///
///
#[proc_macro]
pub fn make_sum_id(input: TokenStream) -> TokenStream {
    make_sum_id_impl(input.into()).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn make_sum_id_test() {
        let input = quote! {
            target_type: Animal,
            id_type: IdAnimal,
            Dog: Dog,
            Cat: Cat,
        };
        let expected_output = quote! {
        #[derive(Debug, Clone, PartialEq, Eq, Hash, LiteralValue)]
        pub enum Animal {
            Dog(Dog),
            Cat(Cat),
        }
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, LiteralValue)]
        pub enum IdAnimal {
            Dog(Id<Dog>),
            Cat(Id<Cat>),
        }
        static_assertions::assert_not_impl_all!(Animal: crate::container::Item);
        impl From<Dog> for Animal {
            fn from(value: Dog) -> Self {
                Animal::Dog(value)
            }
        }
        impl From<Cat> for Animal {
            fn from(value: Cat) -> Self {
                Animal::Cat(value)
            }
        }
        impl IdAnimal {
            pub fn decode<C: Container>(self, container: &C) -> Option<Animal> {
                Some(match self {
                    IdAnimal::Dog(id) => Animal::Dog(id.decode(container)?),
                    IdAnimal::Cat(id) => Animal::Cat(id.decode(container)?),
                })
            }
            pub fn decode_f<C: Container>(self, container: &C) -> Animal {
                match self {
                    IdAnimal::Dog(id) => Animal::Dog(id.decode_f(container)),
                    IdAnimal::Cat(id) => Animal::Cat(id.decode_f(container)),
                }
            }
            pub fn decode_ex<C: Container>(self, container: &C) -> Result<Animal, DecodeError> {
                Ok(match self {
                    IdAnimal::Dog(id) => Animal::Dog(id.decode_ex(container)?),
                    IdAnimal::Cat(id) => Animal::Cat(id.decode_ex(container)?),
                })
            }
        }
        };
        let expected_output = expected_output.to_string();
        fn clean_up(input: String) -> String {
            input.replace(" ", "").replace(",", "")
        }
        let actual_output = make_sum_id_impl(input).to_string();
        assert_eq!(clean_up(expected_output), clean_up(actual_output));
    }
}
