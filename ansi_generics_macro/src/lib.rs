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

enum ExplicitArg {
    Positional(Expr),
    Named { name: Ident, value: Expr },
}
/// ansi_generics!("{}{}", Color::Red.paint("hello "), Color::Blue.paint("kitty!"));
/// Form of struct taken from example at:https://docs.rs/syn/latest/syn/struct.Macro.html?search=parse#method.parse_body
struct FormatArgs {
    fmt_s: LitStr,
    explicit_args: Vec<ExplicitArg>,
}

impl Parse for FormatArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let fmt_s: LitStr = input.parse()?;
        let mut ordered_args = Vec::new();

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
                    ordered_args.push(ExplicitArg::Named { name, value });
                    if input.is_empty() {
                        break;
                    }
                    input.parse::<Token![,]>()?;
                }
                break;
            }
            ordered_args.push(ExplicitArg::Positional(input.parse()?));
        }

        Ok(FormatArgs {
            fmt_s,
            explicit_args: ordered_args,
        })
    }
}

fn extract_inline_named_args(fmt_s: String) -> Vec<String> {
    Parser::new(&fmt_s, false)
        .filter_map(|piece| match piece {
            parse_fmt_str::Piece::String(_) => None,
            parse_fmt_str::Piece::NextArgument(arg) => match arg.position {
                parse_fmt_str::Position::ImplicitlyLocated(_)
                | parse_fmt_str::Position::IndexLocated(_) => None,
                parse_fmt_str::Position::Named(named) => Some(named.to_string()),
            },
        })
        .collect()
}

// Function like proc_macros have signature (TokenStream) -> TokenStream.
#[proc_macro]
pub fn ansi_generics(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let FormatArgs {
        fmt_s,
        mut explicit_args,
    } = parse_macro_input!(input);

    let inline_named_args = extract_inline_named_args(fmt_s.value());
    explicit_args = explicit_args
        .into_iter()
        .filter_map(|arg| match arg {
            x @ ExplicitArg::Positional(_) => Some(x),
            ExplicitArg::Named { name, value } => {
                if inline_named_args.contains(&name.to_string()) {
                    unimplemented!()
                } else {
                    Some(ExplicitArg::Named { name, value })
                }
            }
        })
        .collect();

    quote! {
        $crate::AnsiFormatArgs {
            fmt_s: &'static str,
            fmt_args_producer: F,
            args: $crate::AnsiGenericStrings::from_iter([#(#explicit_args),*]),
        }
    }
    .into()
}
