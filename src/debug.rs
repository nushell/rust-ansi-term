use itertools::Itertools;

use crate::{
    coerce_fmt_write,
    difference::StyleDelta,
    style::{Coloring, Style, StyleFlags},
    write::Content,
    AnsiGenericString, Color, Infix,
};
use std::{
    collections::HashSet,
    fmt::{self, Debug},
};

fn debug_write_style_flags_to(f: &mut dyn fmt::Write, flags: StyleFlags) -> Result<(), fmt::Error> {
    f.write_str(
        &flags
            .iter_names()
            .filter_map(|n| {
                let name = n.0;
                if name == "FOREGROUND" || name == "BACKGROUND" {
                    None
                } else {
                    name.to_lowercase().into()
                }
            })
            .join(", "),
    )
}

fn debug_write_style_to(
    f: &mut dyn fmt::Write,
    flags: StyleFlags,
    fg: Option<Color>,
    bg: Option<Color>,
) -> Result<(), fmt::Error> {
    f.write_str("Style { ")?;

    let mut sep_required = false;
    let mut flag_strings: Vec<String> = flags
        .iter_names()
        .filter_map(|n| {
            let name = n.0;
            if name == "FOREGROUND" || name == "BACKGROUND" {
                None
            } else {
                sep_required = true;
                name.to_lowercase().into()
            }
        })
        .collect();
    flag_strings.sort_unstable();
    f.write_str(&flag_strings.join(", "))?;

    if let Some(color) = fg {
        f.write_fmt(format_args!(
            "{}foreground({color:?})",
            if sep_required { ", " } else { "" }
        ))?;
        sep_required = true;
    }
    if let Some(color) = bg {
        f.write_fmt(format_args!(
            "{}background({color:?})",
            if sep_required { ", " } else { "" }
        ))?;
    }
    f.write_str(" }")
}

fn debug_style_to_string(
    flags: StyleFlags,
    fg: Option<Color>,
    bg: Option<Color>,
) -> Result<String, fmt::Error> {
    let mut dbg_s = String::new();
    let f = coerce_fmt_write!(&mut dbg_s);
    debug_write_style_to(f, flags, fg, bg)?;
    Ok(dbg_s)
}

/// In debug mode, [`Style`]s show flags that are on, and any corresponding
/// colors, if they exist. In alternate debug mode (`:#?`), [`Style`]s show
/// themselves in full detail.
///
///
/// ```
///     use nu_ansi_term::Color::{Red, Blue};
///     assert_eq!(
///         "Style { \
///             flags: StyleFlags { FOREGROUND, BACKGROUND, BOLD, ITALIC },\
///             foreground: Red, background: Blue }",
///        format!("{:?}", Red.with_bg(Blue).bold().italic())
///     );
/// ```
impl Debug for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !f.alternate() {
            f.debug_struct("Style")
                .field("flags", &self.flags)
                .field("coloring", &self.coloring)
                .finish()
        } else {
            write!(
                f,
                "{}",
                debug_style_to_string(
                    self.flags,
                    self.coloring.foreground,
                    self.coloring.background
                )?
            )
        }
    }
}

/// [`StyleFlags`] have a special [`Debug`] implementation that only shows the fields that
/// are set. Fields that haven’t been touched aren’t included in the output.
///
/// This behaviour gets bypassed when using the alternate formatting mode
/// `format!("{:#?}")`.
///
/// ```
///     use nu_ansi_term::Color::{Red, Blue};
///     assert_eq!("Style { foreground(Red),.with_bg(Blue), bold, italic }",
///                format!("{:?}", Red.with_bg(Blue).bold().italic()));
/// ```
impl Debug for StyleFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !f.alternate() {
            let x = StyleFlags::all()
                .iter_names()
                .map(|(name, flag)| format!("{}: {}", name, self.contains(flag)))
                .join(", ");
            f.debug_tuple("StyleFlags").field(&x).finish()
        } else {
            debug_write_style_flags_to(f, *self)
        }
    }
}

pub fn assert_required<T: PartialEq<U> + Eq + Debug, U: PartialEq<T> + Eq + Debug>(
    outcome: U,
    required: T,
    optional: Option<fmt::Arguments<'_>>,
) {
    assert!(
        outcome == required,
        "Disagreement:\n(test) {outcome:?} != {required:?} (required)",
    );
    if let Some(arg) = optional {
        println!("{}", arg);
    }
}

#[allow(unused)]
#[inline]
pub fn test_style_eq(test_style: Style, required: &str) {
    let test = format!("{:#?}", test_style);
    assert_required(test, required, None);
}

#[allow(unused)]
#[inline]
pub fn test_styled_content_eq<P: DebugStylePaint>(test_style: P, content: &str, required: &str)
where
    P: Debug,
{
    let test_result = test_style.style_input(content).to_string();
    let required = required.to_string();
    let required_bytes = required.as_bytes().to_owned();

    assert_required(
        test_result,
        required,
        format_args!("(test_style) {test_style:#?}").into(),
    );

    let mut v = Vec::new();
    test_style
        .style_input(content.as_bytes())
        .write_to(&mut v)
        .unwrap();
    let slice_v = v.as_slice();
    let required = required_bytes;

    assert_required(
        slice_v,
        required,
        format_args!("(test_style) {test_style:#?}").into(),
    );
}

#[allow(unused)]
#[inline]
pub fn test_styled_string_eq(outcome: String, required: &str) {
    assert_required(outcome, required, None);
}

#[macro_export]
macro_rules! style_test {
    (@str_cmp $name: ident: try:$test:expr; req:$req:literal) => {
        #[test]
        fn $name() {
            $crate::debug::test_styled_string_eq($test, $req)
        }
    };
    (@style_eq $name: ident : try:$test:expr; req:($flags:expr, $fg:expr, $bg:expr)) => {
        #[test]
        fn $name() {
            $crate::debug::test_style_eq($test, &debug_style_to_string($flags, $fg, $bg).unwrap())
        }
    };
    (@style_eq $name: ident: try:$test:expr; req:$($req:tt)*) => {
        #[test]
        fn $name() {
            $crate::debug::test_style_eq($test, $($req)*)
        }
    };
    (@content_eq $name: ident: try:$test:expr; content:$content:expr; req:$req:expr) => {
        #[test]
        fn $name() {
            $crate::debug::test_styled_content_eq($test, $content, $req)
        }
    };
}

pub trait DebugStylePaint: Clone + Copy {
    fn into_style(self) -> Style;

    /// Paints the given text with this style, returning an ANSI string.
    #[inline]
    fn style_input<'a, I, S: 'a + ToOwned + ?Sized>(&self, input: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Content<'a, S>>,
        S: fmt::Debug,
    {
        AnsiGenericString::new(self.into_style(), input.into(), None)
    }
}

impl DebugStylePaint for Style {
    fn into_style(self) -> Style {
        self
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FgColor(Color);

impl DebugStylePaint for FgColor {
    fn into_style(self) -> Style {
        self.0.foreground()
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BgColor(Color);

impl DebugStylePaint for BgColor {
    fn into_style(self) -> Style {
        self.0.background()
    }
}

impl DebugStylePaint for Color {
    fn into_style(self) -> Style {
        self.foreground()
    }
}

impl DebugStylePaint for Infix {
    fn into_style(self) -> Style {
        match self.0.compute_delta(self.1) {
            StyleDelta::PrefixUsing(style) => style,
            StyleDelta::Empty => Style::new(),
        }
    }
}

pub trait DebugDiff: Debug + PartialEq + Eq {
    fn debug_diff(&self, expected: &Self) -> String;
}

impl DebugDiff for StyleFlags {
    fn debug_diff(&self, expected: &Self) -> String {
        if self == expected {
            format!("{self:#?} (no diff)")
        } else {
            let outcome_flags = HashSet::<_>::from_iter(self.iter_names().map(|(n, _)| n));
            let expected_flags = HashSet::<_>::from_iter(expected.iter_names().map(|(n, _)| n));
            let in_outcome_not_expected = outcome_flags
                .difference(&expected_flags)
                .copied()
                .collect::<Vec<&str>>();
            let in_expected_not_outcome = expected_flags
                .difference(&outcome_flags)
                .copied()
                .collect::<Vec<&str>>();
            format!(
                "StyleFlags((outcome) {self:#?} (missing: {in_expected_not_outcome:#?}) <~> (missing: {in_outcome_not_expected:#?}) {expected:#?} (expected))",
            )
        }
    }
}

impl DebugDiff for Color {
    fn debug_diff(&self, expected: &Self) -> String {
        if self != expected {
            format!("Color((outcome) {self:#?} != {expected:#?} (expected))",)
        } else {
            format!("{self:#?} (no diff)",)
        }
    }
}

impl<T: DebugDiff> DebugDiff for Option<T> {
    fn debug_diff(&self, expected: &Self) -> String {
        if self == expected {
            format!("{self:#?} (no diff)")
        } else {
            match (self, expected) {
                (None, Some(e)) => {
                    format!("(outcome) {self:#?} != {e:#?} (expected)")
                }
                (Some(out), None) => {
                    format!("(outcome) {out:#?} != {expected:#?} (expected)")
                }
                (Some(o), Some(e)) => {
                    format!("Option<{}>", o.debug_diff(e))
                }
                _ => unreachable!(),
            }
        }
    }
}

impl DebugDiff for Coloring {
    fn debug_diff(&self, expected: &Self) -> String {
        format!(
            "Coloring {{\n\tforeground: {},\n\tbackground: {},\n}}",
            self.foreground.debug_diff(&expected.foreground),
            expected.background.debug_diff(&expected.background)
        )
    }
}

impl DebugDiff for Style {
    fn debug_diff(&self, expected: &Self) -> String {
        format!(
            "Style {{\n\tflags: {},\n\tcoloring: {},\n}}",
            self.flags.debug_diff(&expected.flags),
            self.coloring.debug_diff(&expected.coloring),
        )
    }
}

impl DebugDiff for StyleDelta {
    fn debug_diff(&self, expected: &Self) -> String {
        if self == expected {
            format!("{self:#?} (no diff)")
        } else {
            match (self, expected) {
                (StyleDelta::PrefixUsing(o), StyleDelta::PrefixUsing(e)) => {
                    format!("StyleDelta::PrefixUsing({})", o.debug_diff(e))
                }
                (StyleDelta::PrefixUsing(o), StyleDelta::Empty) => {
                    format!("StyleDelta::PrefixUsing({o:#?} != {expected:#?})")
                }
                (StyleDelta::Empty, StyleDelta::PrefixUsing(e)) => {
                    format!("StyleDelta::PrefixUsing({self:#?} != {e:#?})")
                }
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::debug::debug_style_to_string;
    use crate::style::Color::*;
    use crate::style::{Style, StyleFlags};
    use crate::style_test;
    use paste::paste;

    macro_rules! create_style_eq_tests {
        () => {};
        ([$test_name:ident: $color:expr, $req:literal] $($args:tt)*) => {
            paste! {
                style_test!(
                    @style_eq $test_name:
                    try:$color ;
                    req:$req
                );
            }
            create_style_eq_tests!($($args)*);
        };
        ([$test_name:ident: $($flag:ident),*] $($args:tt)*) => {
            paste! {
                style_test!(
                    @style_eq $test_name:
                    try:Style::new()$(.$flag())* ;
                    req:($(StyleFlags::[< $flag:upper >])|*, None, None)
                );
            }
            create_style_eq_tests!($($args)*);
        };
    }

    style_test!(
        @style_eq empty:
        try:Style::new() ;
        req:(StyleFlags::empty(), None, None)
    );

    create_style_eq_tests!(
        [bold: bold]
        [italic: italic]
        [both: bold, italic]
        [red: Red.foreground(), "Style { foreground(Red) }"]
        [redblue: Red.with_bg(Rgb(3, 2, 4)), "Style { foreground(Red), background(Rgb(3, 2, 4)) }"]
        [everything: Red.with_bg(Blue).blink().bold().dimmed().hidden().italic().reverse().strikethrough().underline(), "Style { blink, bold, dimmed, hidden, italic, reverse, strikethrough, underline, foreground(Red), background(Blue) }"]
    );
}
