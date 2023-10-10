mod parse_fmt_str;
use parse_fmt_str::Parser;
use quote::quote;
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, Ident, LitStr, Result as SynResult, Token,
};

// pub struct AnsiFormatArgs<'a, S: 'a + ToOwned + ?Sized, F>
// where
//     F: Fn(AnsiGenericString<'a, S>) -> fmt::Arguments,
// {
//     pub fmt_s: &'static str,
//     pub args_producer: F,
//     pub args: AnsiGenericStrings<'a, S>,
// }

/// ansi_generics!("{}{}", Color::Red.paint("hello "), Color::Blue.paint("kitty!"));
/// Form of struct taken from example at:https://docs.rs/syn/latest/syn/struct.Macro.html?search=parse#method.parse_body
struct FormatArgs {
    fmt_s: LitStr,
    _positional_args: Vec<Expr>,
    _named_args: Vec<(Ident, Expr)>,
}

impl Parse for FormatArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let fmt_s: LitStr = input.parse()?;
        let mut positional_args = Vec::new();
        let mut named_args = Vec::new();

        while !input.is_empty() {
            input.parse::<Token![,]>()?;
            if input.is_empty() {
                break;
            }
            if input.peek(Ident::peek_any) && input.peek2(Token![=]) {
                while !input.is_empty() {
                    let name: Ident = input.call(Ident::parse_any)?;
                    input.parse::<Token![=]>()?;
                    let value: Expr = input.parse()?;
                    named_args.push((name, value));
                    if input.is_empty() {
                        break;
                    }
                    input.parse::<Token![,]>()?;
                }
                break;
            }
            positional_args.push(input.parse()?);
        }

        Ok(FormatArgs {
            fmt_s,
            _positional_args: positional_args,
            _named_args: named_args,
        })
    }
}

fn extract_inline_args(fmt_s: String) -> Vec<String> {
    Parser::new(&fmt_s, None, false, mode)
}

// Function like proc_macros have signature (TokenStream) -> TokenStream.
#[proc_macro]
pub fn ansi_generics(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let FormatArgs {
        fmt_s,
        _positional_args: _,
        _named_args: _,
    } = parse_macro_input!(input);

    let _inline_args = extract_inline_args(fmt_s.value());

    quote! {
        $crate::AnsiFormatArgs {
            fmt_s: &'static str,
            pub args_producer: F,
            pub args: AnsiGenericStrings<'a, S>,
        }
    }
    .into()
}
