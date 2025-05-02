//! A macro that provides a for-else control structure in Rust.
//! 
//! This macro allows you to execute code in an else block when a for loop completes without breaking.
//! It's similar to Python's for-else construct.
//! 
//! # Examples
//! 
//! ```rust
//! use forelse::for_else;
//! 
//! let mut found = false;
//! for_else!(x in 1..=5 => {
//!     if x == 3 {
//!         println!("Found 3");
//!         found = true;
//!     }
//! } else {
//!     println!("Not found");
//! });
//! ```
//! 
//! The else block will execute if the loop completes without breaking.
//! This is useful for search operations where you want to handle both found and not found cases.

use quote::quote;
use syn::{parse_macro_input, Expr, Block, Pat};

/// The main macro that implements the for-else control structure.
/// 
/// # Syntax
/// 
/// ```rust
/// use forelse::for_else;
/// 
/// for_else!(x in 1..=5 => {
///     // loop body
/// } else {
///     // else body
/// });
/// ```
/// 
/// The else block will execute if the loop completes without breaking.
#[proc_macro]
pub fn for_else(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ForElseInput);
    
    let pattern = input.pattern;
    let range = input.range;
    let body = input.body;
    let else_body = input.else_body;
    
    let expanded = quote! {
        {
            let mut executed = false;
            {
                'for_else: for #pattern in #range {
                    executed = true;
                    #body
                }
            }
            if !executed {
                #else_body
            }
        }
    };
    
    proc_macro::TokenStream::from(expanded)
}

/// Internal structure to parse the macro input
struct ForElseInput {
    pattern: Pat,
    range: Expr,
    body: Block,
    else_body: Block,
}

impl syn::parse::Parse for ForElseInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pattern = Pat::parse_single(input)?;
        input.parse::<syn::Token![in]>()?;
        let range = Expr::parse(input)?;
        input.parse::<syn::Token![=>]>()?;
        let body = Block::parse(input)?;
        input.parse::<syn::Token![else]>()?;
        let else_body = Block::parse(input)?;
        
        Ok(ForElseInput {
            pattern,
            range,
            body,
            else_body,
        })
    }
}
