use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use std::fmt::Error;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, ExprLit,
    GenericArgument, Ident, Lit, Meta, MetaList, MetaNameValue, Path, Type, TypePath, WhereClause,
};

struct W<T>(T);

impl TryFrom<W<(String, proc_macro2::Span)>> for WhereClause {
    type Error = Error;
    fn try_from(W((string, span)): W<(String, proc_macro2::Span)>) -> Result<Self, Self::Error> {
        let mut bound = string.split("::");
        let name = bound.next().unwrap().replace("\"", "");
        let generic = Ident::new(name.as_str(), span);
        let mut bound = bound.next().unwrap().split(":");
        let assoc = Ident::new(bound.next().unwrap(), span);
        Ok(parse_quote! { where #generic::#assoc: std::fmt::Debug })
    }
}

fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

#[rustfmt::skip]
fn attr_is_debug(f: &syn::Field) -> Option<String> {
    if f.attrs.is_empty() {
        return None;
    }
    if let Meta::NameValue(MetaNameValue {
        path: Path { segments, .. },
        value: Expr::Lit(ExprLit { lit: Lit::Str(lit), .. }), .. }) = &f.attrs[0].meta
    {
        if &segments[0].ident.to_string() == "debug" {
            return Some(lit.token().to_string());
        }
    }
    None
}

#[rustfmt::skip]
fn attr_is_bound(attrs: &Vec<syn::Attribute>) -> Option<(String, proc_macro2::Span)> {
    for attr in attrs {
        if let Attribute {meta: Meta::List(MetaList { path, .. }),.. } = &attr {
            if &path.segments[0].ident.to_string() != "debug" {
                return None;
            };
        }
        if let Attribute { meta: Meta::List(meta), .. } = &attr {
            for token in meta.tokens.clone().into_iter() {
                if let TokenTree::Literal(lit) = token {
                    return Some((lit.to_string(), lit.span()));
                }
            }
        }
    }
    None
}

fn has_assoc_ty(fields: &Punctuated<syn::Field, syn::token::Comma>) -> bool {
    for f in fields {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &f.ty {
            if let syn::PathArguments::AngleBracketed(arguments) = &path.segments[0].arguments {
                if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { path, .. })) =
                    &arguments.args[0]
                {
                    if path.segments.len() >= 2 {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn generic_in_phantom(fields: &Punctuated<syn::Field, syn::token::Comma>) -> bool {
    for f in fields {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &f.ty {
            let ident_outer = &path.segments[0].ident;
            if let syn::PathArguments::AngleBracketed(generic) = &path.segments[0].arguments {
                if let GenericArgument::Type(Type::Path(TypePath { path, .. })) = &generic.args[0] {
                    let _ident_inner = &path.segments[0].ident;
                    if *ident_outer == "PhantomData" {
                        return true;
                    }
                }
            }
        }
    }
    false
}

fn build_where_clause(
    fields: &Punctuated<syn::Field, syn::token::Comma>,
) -> Option<syn::WhereClause> {
    for f in fields {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &f.ty {
            if let syn::PathArguments::AngleBracketed(arguments) = &path.segments[0].arguments {
                if let GenericArgument::Type(Type::Path(TypePath { path, .. })) = &arguments.args[0]
                {
                    if path.segments.len() >= 2 {
                        let generic = &path.segments[0].ident;
                        let assoc = &path.segments[1].ident;
                        return Some(parse_quote! {
                            where #generic::#assoc: std::fmt::Debug
                        });
                    }
                }
            }
        }
    }
    None
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;

    let fields = match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected struct with named fields"),
    };

    let pair = fields.into_iter().map(|f| {
        let ident = &f.ident;
        if let Some(format) = attr_is_debug(f) {
            let format = &format[1..format.len() - 1];
            quote! { .field(stringify!(#ident), &format_args!(#format, &self.#ident)) }
        } else {
            quote! { .field(stringify!(#ident), &self.#ident) }
        }
    });

    let attr_is_bound = attr_is_bound(&ast.attrs);
    let generics = if has_assoc_ty(fields) || generic_in_phantom(fields) || attr_is_bound.is_some()
    {
        ast.generics
    } else {
        add_trait_bounds(ast.generics)
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let where_clause = if let Some(data) = attr_is_bound {
        W(data).try_into().ok()
    } else {
        build_where_clause(fields).or_else(|| where_clause.cloned())
    };

    quote! {
        impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#ident))
                    #(#pair)*
                    .finish()
            }
        }
    }
    .into()
}
