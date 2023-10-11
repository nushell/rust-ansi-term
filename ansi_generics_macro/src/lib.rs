mod parse_fmt_str;
use parse_fmt_str::{Argument, Parser, Piece, Position};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use rand::prelude::*;
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote, Expr, Ident, LitStr, Result as SynResult, Token,
};

// pub struct AnsiFormatArgs<'a, S: 'a + ToOwned + ?Sized, F>
// where
//     F: Fn(AnsiGenericString<'a, S>) -> fmt::Arguments,
// {
//     pub fmt_s: &'static str,
//     pub args_producer: F,
//     pub args: AnsiGenericStrings<'a, S>,
// }

enum InputExplicit {
    Positional {
        expr: Expr,
    },
    Named {
        name: Ident,
        expr: Expr,
        str_name: String,
    },
}

impl Parse for InputExplicit {
    fn parse(input: ParseStream) -> SynResult<Self> {
        if input.peek(Ident::peek_any) && input.peek2(Token![=]) {
            let name: Ident = input.call(Ident::parse_any)?;
            input.parse::<Token![=]>()?;
            let value: Expr = input.parse()?;
            Ok(InputExplicit::Named {
                name,
                expr: value,
                str_name: name.to_string(),
            })
        } else {
            Ok(InputExplicit::Positional {
                expr: input.parse()?,
            })
        }
    }
}

/// ansi_generics!("{}{}", Color::Red.paint("hello "), Color::Blue.paint("kitty!"));
/// Form of struct taken from example at:https://docs.rs/syn/latest/syn/struct.Macro.html?search=parse#method.parse_body
struct InputArgs {
    fmt_s: LitStr,
    explicit_args: Vec<DiscoveredExplicit>,
}

struct DiscoveredExplicit {
    matched: bool,
    explicit: InputExplicit,
}

impl Parse for InputArgs {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let fmt_s: LitStr = input.parse()?;
        let mut explicit_args = Vec::new();
        let mut arg_ix = 0;

        while !input.is_empty() {
            input.parse::<Token![,]>()?;
            if input.is_empty() {
                break;
            }
            explicit_args.push(DiscoveredExplicit {
                matched: false,
                explicit: input.parse()?,
            });
        }

        Ok(InputArgs {
            fmt_s,
            explicit_args,
        })
    }
}

enum ExplicitName {
    Generated(Ident),
    Given(Ident),
}

impl From<&ExplicitName> for &Ident {
    fn from(explicit_name: &ExplicitName) -> Self {
        match explicit_name {
            ExplicitName::Generated(id) | ExplicitName::Given(id) => id,
        }
    }
}

impl ExplicitName {
    fn new_random() -> Self {
        ExplicitName::Generated({
            let r: u64 = random();
            parse_quote!(format!("x{}", r))
        })
    }
}

impl ToTokens for ExplicitName {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let id: &Ident = self.into();
        id.to_tokens(tokens);
    }
}

struct MatchedExplicit {
    name: ExplicitName,
    expr: Expr,
    position: usize,
}

enum MatchResult {
    MatchFound(MatchedExplicit),
    AlreadyMatched,
    ImplicitNamed(Ident),
}

impl MatchedExplicit {
    fn with_random_name(position: usize, expr: Expr) -> Self {
        Self {
            name: ExplicitName::new_random(),
            expr,
            position,
        }
    }

    fn with_name(name: Ident, position: usize, expr: Expr) -> Self {
        Self {
            name: ExplicitName::Given(name),
            expr,
            position,
        }
    }

    fn match_with_arg(
        argument: &Argument<'_>,
        explicits: &mut [DiscoveredExplicit],
    ) -> MatchResult {
        match argument.position {
            Position::ImplicitlyLocated(ix) | Position::IndexGiven(ix) => explicits
                .get(ix)
                .map(|d| match d.explicit {
                    InputExplicit::Positional { expr } => {
                        if !d.matched {
                            d.matched = true;
                            MatchResult::MatchFound(MatchedExplicit::with_random_name(
                                ix,
                                expr.clone(),
                            ))
                        } else {
                            MatchResult::AlreadyMatched
                        }
                    }
                    InputExplicit::Named { .. } => {
                        panic!("Expected a positional, unnamed argument.")
                    }
                })
                .expect("arguments without name should have some explicit match"),
            Position::Named(this) => explicits
                .iter()
                .enumerate()
                .find_map(|(ix, d)| match d.explicit {
                    InputExplicit::Positional { expr } => panic!("Expected a named argument."),
                    InputExplicit::Named {
                        name,
                        expr,
                        str_name,
                    } => (this == str_name).then(|| {
                        if d.matched {
                            MatchResult::AlreadyMatched
                        } else {
                            MatchResult::MatchFound(MatchedExplicit::with_name(
                                name.clone(),
                                ix,
                                expr.clone(),
                            ))
                        }
                    }),
                })
                .unwrap_or(MatchResult::ImplicitNamed(Ident::new(
                    this,
                    Span::call_site(),
                ))),
        }
    }
}

enum Arg {
    Explicit(MatchedExplicit),
    ImplicitNamed(Ident),
}

impl Arg {
    fn from_fmt_arg(arg: &Argument<'_>, explicits: &mut [DiscoveredExplicit]) -> Option<Arg> {
        match MatchedExplicit::match_with_arg(arg, explicits) {
            MatchResult::MatchFound(explicit) => Arg::Explicit(explicit).into(),
            MatchResult::ImplicitNamed(name) => Arg::ImplicitNamed(name).into(),
            MatchResult::AlreadyMatched => None,
        }
    }
}

struct FormatArgs {
    args: Vec<Arg>,
    explicit_arg_tokens: Vec<TokenStream2>,
}

impl FormatArgs {
    fn new(args: Vec<Arg>) -> Self {
        let explicit_arg_tokens = args
            .iter()
            .filter_map(|x| match x {
                Arg::Explicit(MatchedExplicit {
                    name,
                    expr,
                    position,
                }) => match name {
                    ExplicitName::Generated(_) => quote! {
                        #expr
                    }
                    .into(),
                    ExplicitName::Given(name) => quote! {
                        #name=#expr
                    }
                    .into(),
                },
                _ => None,
            })
            .collect();

        FormatArgs {
            args,
            explicit_arg_tokens,
        }
    }

    fn all_names(&self) -> impl Iterator<Item = &Ident> {
        self.args.iter().map(|arg| match arg {
            Arg::Explicit(MatchedExplicit { name, .. }) => name.into(),
            Arg::ImplicitNamed(name) => name,
        })
    }

    fn explicit_arg_tokens(&self) -> &[TokenStream2] {
        &self.explicit_arg_tokens
    }
}

fn extract_inline_named_args(
    mut explicit_args: Vec<DiscoveredExplicit>,
    fmt_s: String,
) -> FormatArgs {
    FormatArgs::new(
        Parser::new(&fmt_s, false)
            .filter_map(|piece| match piece {
                parse_fmt_str::Piece::String(_) => None,
                parse_fmt_str::Piece::NextArgument(arg) => {
                    Arg::from_fmt_arg(arg.as_ref(), &mut explicit_args)
                }
            })
            .collect(),
    )
}

// Function like proc_macros have signature (TokenStream) -> TokenStream.
#[proc_macro]
pub fn ansi_generics(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let InputArgs {
        fmt_s,
        explicit_args,
    } = parse_macro_input!(input);

    let format_args = extract_inline_named_args(explicit_args, fmt_s.value());

    // Remove any inline named args that are specified explicitly, e.g.:
    // format!("hello {kitty}!", kitty="Gamsiz");
    explicit_args.iter().for_each(|arg| match arg {
        InputExplicit::Positional(_) => {}
        InputExplicit::Named { name, .. } => {
            if let Some(position) = inline_named_args.iter().position(|x| x == name) {
                inline_named_args.remove(position);
            }
        }
    });

    quote! {
        let ansi_arg_strings = $crate::AnsiGenericStrings::from_iter([])
        $crate::AnsiFormatArgs {
            fmt_args_producer: |#(#explicit_arg_names)|,
            args: $crate::AnsiGenericStrings::from_iter([#(#explicit_args),* #(#inline_named_args),*]),
        }
    }
    .into()
}
