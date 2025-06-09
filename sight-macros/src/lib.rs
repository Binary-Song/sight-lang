use proc_macro::TokenStream;
use quote::quote;
use syn;
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
                        let field_names = fields_named.named.iter().map(|f| f.ident.as_ref().unwrap());
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

// 这样你就可以在其他 crate 里这样用：
// #[derive(MyDerive)]
// struct MyStruct {}
