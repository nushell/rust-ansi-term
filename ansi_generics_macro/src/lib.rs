mod parse_fmt_str;
use std::cmp::Ordering;

use parse_fmt_str::{Argument, Parser, Position};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use rand::prelude::*;
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
                str_name: name.to_string(),
                name,
                expr: value,
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

#[derive(Clone, Debug, PartialEq, Eq)]
enum ExplicitName {
    Generated(Ident),
    Given(Ident),
}

impl ExplicitName {
    fn new_random() -> Self {
        ExplicitName::Generated({
            let r: u64 = random();
            format_ident!("x{}", r)
        })
    }

    fn id(&self) -> &Ident {
        match self {
            ExplicitName::Generated(id) | ExplicitName::Given(id) => id,
        }
    }
}

impl ToTokens for ExplicitName {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.id().to_tokens(tokens)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
                .get_mut(ix)
                .map(|d: &mut DiscoveredExplicit| match &mut d.explicit {
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
                .find_map(|(ix, d)| match &d.explicit {
                    InputExplicit::Positional { .. } => panic!("Expected a named argument."),
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

    fn id(&self) -> &Ident {
        self.name.id()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    fn filter_implicit(&self) -> Option<&Ident> {
        match self {
            Arg::Explicit(_) => None,
            Arg::ImplicitNamed(id) => Some(id),
        }
    }

    fn filter_explicit(&self) -> Option<&MatchedExplicit> {
        match self {
            Arg::Explicit(explicit) => explicit.into(),
            _ => None,
        }
    }
}

struct FormatArgs {
    args: Vec<Arg>,
}

impl FormatArgs {
    fn new(mut args: Vec<Arg>) -> Self {
        args.sort_by(|a, b| match (a, b) {
            (Arg::Explicit(x), Arg::Explicit(y)) => x.position.cmp(&y.position),
            (Arg::Explicit(_), Arg::ImplicitNamed(_)) => Ordering::Less,
            (Arg::ImplicitNamed(_), Arg::Explicit(_)) => Ordering::Greater,
            (Arg::ImplicitNamed(_), Arg::ImplicitNamed(_)) => Ordering::Equal,
        });
        FormatArgs { args }
    }

    fn all_names(&self) -> impl Iterator<Item = &Ident> {
        self.args.iter().map(|arg| match arg {
            Arg::Explicit(x) => x.id(),
            Arg::ImplicitNamed(name) => name,
        })
    }

    fn inline_names(&self) -> impl Iterator<Item = &Ident> {
        self.args.iter().filter_map(Arg::filter_implicit)
    }

    fn explicit_args(&self) -> impl Iterator<Item = &MatchedExplicit> {
        self.args.iter().filter_map(Arg::filter_explicit)
    }

    fn explicit_arg_tokens(&self) -> impl Iterator<Item = TokenStream2> + '_ {
        self.args
            .iter()
            .enumerate()
            .filter_map(|(ix, arg)| match arg {
                Arg::Explicit(MatchedExplicit { name, .. }) => match name {
                    ExplicitName::Generated(_) => quote!(inputs[#ix]),
                    ExplicitName::Given(_) => quote!(#name=inputs[#ix]),
                }
                .into(),
                Arg::ImplicitNamed(_) => None,
            })
    }

    fn explicit_exprs(&self) -> impl Iterator<Item = Expr> + '_ {
        self.explicit_args().map(|explicit| explicit.expr.clone())
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
    let all_names: Vec<&Ident> = format_args.all_names().collect();
    let n_inputs = all_names.len();
    let explicit_arg_tokens: Vec<TokenStream2> = format_args.explicit_arg_tokens().collect();
    let explicit_exprs: Vec<Expr> = format_args.explicit_exprs().collect();
    let inline_names: Vec<&Ident> = format_args.inline_names().collect();

    quote! {
        {
            #[inline]
            fn ___create_ansi_format_args___<'a, S: 'a + ?Sized + ToOwned, F: Fn([AnsiGenericString<'a, S>; #n_inputs]) -> std::fmt::Arguments<'a>>(#(#all_names: AnsiGenericString<'a, S>),*) -> AnsiFormatArgs<'a, S, F, #n_inputs> {
                AnsiFormatArgs {
                    args_producer: |inputs: [AnsiGenericString<'a, S>; #n_inputs]| -> std::fmt::Arguments<'a> {
                        format_args!(#fmt_s, #(#explicit_arg_tokens),*)
                    },
                    producer_inputs: [#(#all_names),*],
                }
            }

            ___create_ansi_format_args___(#(#explicit_exprs),* #(#inline_names),*)
        }
    }
    .into()
}
