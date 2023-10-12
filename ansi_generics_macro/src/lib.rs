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

// pub struct AnsiFormatArgs<'a, F>
// where
//     F: Fn(AnsiGenericString<'a, str>) -> fmt::Arguments,
// {
//     pub fmt_s: &'static str,
//     pub args_producer: F,
//     pub args: AnsiGenericStrings<'a, str>,
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

    fn inline_names(&self) -> impl Iterator<Item = &Ident> {
        self.args.iter().filter_map(Arg::filter_implicit)
    }

    fn explicit_args(&self) -> impl Iterator<Item = &MatchedExplicit> {
        self.args.iter().filter_map(Arg::filter_explicit)
    }

    fn arg_tokens(&self) -> impl Iterator<Item = TokenStream2> + '_ {
        self.args
            .iter()
            .enumerate()
            .filter_map(|(ix, arg)| match arg {
                Arg::Explicit(MatchedExplicit { name, .. }) => match name {
                    ExplicitName::Generated(_) => quote!(render_inputs[#ix]),
                    ExplicitName::Given(_) => quote!(#name=render_inputs[#ix]),
                }
                .into(),
                Arg::ImplicitNamed(_) => None,
            })
    }

    fn string_exprs(&self) -> TokenStream2 {
        let string_exprs = self
            .explicit_args()
            .map(|explicit| explicit.expr.to_token_stream())
            .chain(self.inline_names().map(|name| name.to_token_stream()));
        quote!(#(AnsiGenericString::from(#string_exprs)),*)
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
    let arg_tokens: Vec<TokenStream2> = format_args.arg_tokens().collect();
    let string_exprs = format_args.string_exprs();
    let random_id = format_ident!("GenericFmtArgImplementor{}", random::<usize>());
    let format_args = quote!(format!(#fmt_s, #(#arg_tokens),*));
    let fmt_args_str = format!("Some({})", format_args);
    let random_id_str = format!("{}", random_id);

    quote! {
        {
            use nu_ansi_term::{FmtArgRenderer, AnsiGenericString};
            use std::fmt;

            #[derive(Default)]
            struct #random_id<'a>  {
                render_inputs: Vec<AnsiGenericString<'a, str>>,
            }

            impl<'a> #random_id<'a>  {
                fn new_boxed(render_inputs: Vec<AnsiGenericString<'a, str>>) -> Box<dyn FmtArgRenderer<str>> {
                    Box::new(Self {
                        render_inputs,
                    })
                }
            }

            impl<'a> FmtArgRenderer<'a, str> for #random_id<'a> where Self: 'a + Clone
            {
                fn render_inputs_ref(&self) -> &[AnsiGenericString<'a, str>] {
                    &self.render_inputs
                }

                fn render_inputs_mut(&mut self) -> &mut [AnsiGenericString<'a, str>] {
                    &mut self.render_inputs
                }

                fn clone_renderer(&self) -> Box<dyn FmtArgRenderer<'a, str>> {
                    Box::new(self.clone())
                }

                fn render(&self) -> String {
                    let render_inputs = self.render_inputs_ref();
                    #format_args
                }
            }

            impl<'a> Clone for #random_id<'a>  {
                fn clone(&self) -> Self {
                    Self {
                        render_inputs: self.render_inputs.clone(),
                    }
                }
            }

            impl<'a> fmt::Debug for #random_id<'a>
            {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.debug_struct(#random_id_str)
                        .field("render_inputs", &self.render_inputs)
                        .field(
                            "fmt_args", &#fmt_args_str
                        ).finish()
                }
            }

            #random_id::new_boxed(vec![#string_exprs])
        }
    }
    .into()
}
