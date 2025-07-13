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
pub fn derive_internable(input: TokenStream) -> TokenStream  {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let gen = quote! {
        impl crate::utils::interning::Internable for #name {}
    };
    gen.into()
}
