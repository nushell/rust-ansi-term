use std::hash::Hash;

use bitflags::bitflags;
use paste::paste;

bitflags! {
    #[derive(Clone, Copy, Default, PartialEq, Eq)]
    pub struct StyleFlags: u16 {
        /// Whether this style is always displayed starting with a reset code to clear any remaining style artifacts
        const PREFIX_WITH_RESET = 1 << 0;
        /// The style's foreground color; if it has one.
        const FOREGROUND = 1 << 1;
        /// The style's background color; if it has one.
        const BACKGROUND = 1 << 2;
        /// Whether this style is bold.
        const BOLD = 1 << 3;
        /// Whether this style is dimmed.
        const DIMMED = 1 << 4;
        /// Whether this style is italic.
        const ITALIC = 1 << 5;
        /// Whether this style is underlined.
        const UNDERLINE = 1 << 6;
        /// Whether this style is blinking.
        const BLINK = 1 << 7;
        /// Whether this style has reverse colors.
        const REVERSE = 1 << 8;
        /// Whether this style is hidden.
        const HIDDEN = 1 << 9;
        /// Whether this style is struckthrough.
        const STRIKETHROUGH = 1 << 10;
    }
}

impl StyleFlags {
    #[inline]
    pub fn has_color(self) -> bool {
        self.contains(StyleFlags::FOREGROUND | StyleFlags::BACKGROUND)
    }

    /// Get the flags which are unrelated to color or prefixing with a reset,
    /// and are only involved in formatting the output in some manner.
    #[inline]
    pub fn format_flags(self) -> StyleFlags {
        self.intersection(
            (StyleFlags::FOREGROUND | StyleFlags::BACKGROUND | StyleFlags::PREFIX_WITH_RESET)
                .complement(),
        )
    }

    /// Check if the style contains some property which cannot be inverted, and
    /// thus must be followed by a reset flag in order turn off its effect.
    #[inline]
    pub fn has_formatting(self) -> bool {
        !self.format_flags().is_empty()
    }

    /// Check which properties were turned on in the next style.
    #[inline]
    pub const fn turned_on(before: StyleFlags, after: StyleFlags) -> StyleFlags {
        before.complement().intersection(after)
    }

    /// Check which properties were turned off in the next style.
    #[inline]
    pub const fn turned_off(before: StyleFlags, after: StyleFlags) -> StyleFlags {
        before.intersection(after.complement())
    }

    #[inline]
    pub fn set_flags(mut self, flags: StyleFlags) -> Self {
        self.insert(flags);
        self
    }

    #[inline]
    pub fn unset_flags(mut self, flags: StyleFlags) -> Self {
        self &= !flags;
        self
    }

    #[inline]
    pub fn is_format_prop(&self) -> bool {
        !matches!(
            *self,
            StyleFlags::PREFIX_WITH_RESET | StyleFlags::FOREGROUND | StyleFlags::BACKGROUND
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Coloring {
    pub foreground: Option<Color>,
    pub background: Option<Color>,
}

/// A style is a collection of properties that can format a string
/// using ANSI escape codes.
///
/// # Examples
///
/// ```
/// use nu_ansi_term::{Style, Color};
///
/// let style = Style::new().bold().background(Color::Black);
/// println!("{}", style.paint("Bold on black"));
/// ```
#[derive(Clone, Copy)]
#[cfg_attr(
    feature = "derive_serde_style",
    derive(serde::Deserialize, serde::Serialize)
)]
pub struct Style {
    pub flags: StyleFlags,
    pub coloring: Coloring,
}

impl PartialEq for Style {
    fn eq(&self, other: &Self) -> bool {
        self.flags.symmetric_difference(other.flags).is_empty()
            && self.is_foreground() == other.is_foreground()
            && self.is_background() == other.is_background()
    }
}

impl Eq for Style {}

impl Default for Style {
    /// Returns a style with *no* properties set. Formatting text using this
    /// style returns the exact same text.
    ///
    /// ```
    /// use nu_ansi_term::Style;
    /// assert_eq!(None,  Style::default().is_foreground());
    /// assert_eq!(None,  Style::default().is_background());
    /// assert_eq!(false, Style::default().is_bold());
    /// assert_eq!("txt", Style::default().paint("txt").to_string());
    /// ```
    fn default() -> Self {
        Style {
            flags: StyleFlags::empty(),
            coloring: Coloring::default(),
        }
    }
}

macro_rules! style_methods_for_flag {
    (@color $flag:ident) => {
        paste! {
            #[doc = r"If a color is provided: returns a copy of this style with the [`StyleFlags::`" $flag r"`] property set, and the corresponding color value set to the provided color."]
            #[doc = r"Otherwise, it unsets the property flag, and sets the underlying color to `None`."]
            #[doc = r""]
            #[doc = r"# Examples"]
            #[doc = r""]
            #[doc = r"```"]
            #[doc = r"use nu_ansi_term::Style;"]
            #[doc = r"use nu_ansi_term::Color;"]
            #[doc = r""]
            #[doc = r"let style = Style::new()." [< $flag:lower >] r"(Color::Red);"]
            #[doc = r#"println!("{}", style.paint("hey"));"# ]
            #[doc = r"```"]
            #[inline]
            pub fn [< set_ $flag:lower >](mut self, color: Option<Color>) -> Self {
                if self.[<is_ $flag:lower>]() != color {
                    let relevant_flag = StyleFlags::[< $flag >];
                    if color.is_some() {
                        self.flags.insert(relevant_flag)
                    } else {
                        self.unset_flags(relevant_flag);
                    }
                    self.coloring.[< $flag:lower >] = color;
                }
                self
            }

            #[inline]
            pub fn [< $flag:lower >](&self, color: Color) -> Self {
                self.[< set_ $flag:lower >](color.into())
            }

            #[doc = r"Checks if the [`StyleFlags::`" $flag r"`] property is set, and returns the corresponding color if so."]
            pub const fn [< is_ $flag:lower >](&self) -> Option<Color> {
                if self.flags.contains(StyleFlags::$flag) {
                    self.coloring.[< $flag:lower >]
                } else {
                    None
                }
            }
        }
    };
    (FOREGROUND) => {
        style_methods_for_flag!(@color FOREGROUND);
    };
    (BACKGROUND) => {
        style_methods_for_flag!(@color BACKGROUND);
    };
    // A non-color related flag.
    ($flag:ident) => {
        paste! {
            #[doc = r"Returns a copy of this style with the [`StyleFlags::`" $flag r"`] property set"]
            #[doc = r""]
            #[doc = r"# Examples"]
            #[doc = r""]
            #[doc = r"```"]
            #[doc = r"use nu_ansi_term::Style;"]
            #[doc = r""]
            #[doc = r"let style = Style::new()." [< $flag:lower >] r"();"]
            #[doc = r#"println!("{}", style.paint("hey"));"# ]
            #[doc = r"```"]
            pub fn [< $flag:lower >](&self) -> Style {
                (*self).set_flags(StyleFlags::$flag)
            }

            #[doc = r"Checks if the [`StyleFlags::`" $flag r"`] property is set."]
            pub const fn [< is_ $flag:lower >](&self) -> bool {
                self.flags.contains(StyleFlags::$flag)
            }

            #[doc = r"Returns a copy of this style with the [`StyleFlags::`" $flag r"`] property unset."]
            pub fn [< without_ $flag:lower >](&self) -> Style {
                (*self).unset_flags(StyleFlags::$flag)
            }
        }
    }
}

macro_rules! style_methods {
    ($($flag:ident),*) => {
        $(style_methods_for_flag!($flag);)*
    };
}

impl Style {
    /// Creates a new Style with no properties set.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Style;
    ///
    /// let style = Style::new();
    /// println!("{}", style.paint("hi"));
    /// ```
    pub fn new() -> Style {
        Style::default()
    }

    pub fn set_flags(mut self, flags: StyleFlags) -> Self {
        self.flags.insert(flags);
        self
    }

    pub fn unset_flags(mut self, flags: StyleFlags) -> Self {
        self.flags &= !flags;
        self
    }

    #[inline]
    pub fn with_flags(&self, flags: StyleFlags) -> Style {
        (*self).set_flags(flags)
    }

    #[inline]
    pub fn without_flags(&self, flags: StyleFlags) -> Style {
        (*self).unset_flags(flags)
    }

    style_methods!(
        PREFIX_WITH_RESET,
        BOLD,
        DIMMED,
        ITALIC,
        UNDERLINE,
        BLINK,
        REVERSE,
        HIDDEN,
        STRIKETHROUGH,
        FOREGROUND,
        BACKGROUND
    );

    /// Return true if this `Style` requires no escape codes to be represented.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Style;
    ///
    /// assert_eq!(true,  Style::default().is_empty());
    /// assert_eq!(false, Style::default().bold().is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.flags.is_empty()
    }

    #[inline]
    pub fn has_no_styling(&self) -> bool {
        self.without_prefix_with_reset().is_empty()
    }

    #[inline]
    pub fn has_color(&self) -> bool {
        self.flags.has_color()
    }

    /// Get the flags which are unrelated to color or prefixing with a reset,
    /// and are only involved in formatting the output in some manner.
    #[inline]
    pub fn format_flags(&self) -> StyleFlags {
        self.flags.format_flags()
    }

    /// Check if the style contains some property which cannot be inverted, and
    /// thus must be followed by a reset flag in order turn off its effect.
    #[inline]
    pub fn has_non_invertible(&self) -> bool {
        self.flags.has_formatting()
    }

    /// Check which properties were turned off in the next style.
    #[inline]
    pub const fn turned_off(before: Style, after: Style) -> StyleFlags {
        StyleFlags::turned_off(before.flags, after.flags)
    }

    /// Return which properties were turned on in this style, compared to the other.
    #[inline]
    pub const fn turned_on(before: Style, after: Style) -> StyleFlags {
        StyleFlags::turned_on(before.flags, after.flags)
    }

    pub fn set_coloring(self, coloring: Coloring) -> Self {
        self.set_foreground(coloring.foreground)
            .set_background(coloring.background)
    }

    pub fn coloring(&self, coloring: Coloring) -> Self {
        (*self).set_coloring(coloring)
    }

    pub fn iter_formats(&self) -> impl Iterator<Item = (&'_ str, StyleFlags)> {
        self.flags
            .iter_names()
            .filter_map(|(name, flag)| flag.is_format_prop().then_some((name, flag)))
    }
}

// ---- colors ----

/// A color is one specific type of ANSI escape code, and can refer
/// to either the foreground or background color.
///
/// These use the standard numeric sequences.
/// See <http://invisible-island.net/xterm/ctlseqs/ctlseqs.html>
#[derive(Eq, PartialEq, Clone, Copy, Debug, Default)]
#[cfg_attr(
    feature = "derive_serde_style",
    derive(serde::Deserialize, serde::Serialize)
)]
pub enum Color {
    /// Color #0 (foreground code `30`, background code `40`).
    ///
    /// This is not necessarily the background color, and using it as one may
    /// render the text hard to read on terminals with dark backgrounds.
    Black,

    /// Color #0 (foreground code `90`, background code `100`).
    DarkGray,

    /// Color #1 (foreground code `31`, background code `41`).
    Red,

    /// Color #1 (foreground code `91`, background code `101`).
    LightRed,

    /// Color #2 (foreground code `32`, background code `42`).
    Green,

    /// Color #2 (foreground code `92`, background code `102`).
    LightGreen,

    /// Color #3 (foreground code `33`, background code `43`).
    Yellow,

    /// Color #3 (foreground code `93`, background code `103`).
    LightYellow,

    /// Color #4 (foreground code `34`, background code `44`).
    Blue,

    /// Color #4 (foreground code `94`, background code `104`).
    LightBlue,

    /// Color #5 (foreground code `35`, background code `45`).
    Purple,

    /// Color #5 (foreground code `95`, background code `105`).
    LightPurple,

    /// Color #5 (foreground code `35`, background code `45`).
    Magenta,

    /// Color #5 (foreground code `95`, background code `105`).
    LightMagenta,

    /// Color #6 (foreground code `36`, background code `46`).
    Cyan,

    /// Color #6 (foreground code `96`, background code `106`).
    LightCyan,

    /// Color #7 (foreground code `37`, background code `47`).
    ///
    /// As above, this is not necessarily the foreground color, and may be
    /// hard to read on terminals with light backgrounds.
    White,

    /// Color #7 (foreground code `97`, background code `107`).
    LightGray,

    /// A color number from 0 to 255, for use in 256-color terminal
    /// environments.
    ///
    /// - colors 0 to 7 are the `Black` to `White` variants respectively.
    ///   These colors can usually be changed in the terminal emulator.
    /// - colors 8 to 15 are brighter versions of the eight colors above.
    ///   These can also usually be changed in the terminal emulator, or it
    ///   could be configured to use the original colors and show the text in
    ///   bold instead. It varies depending on the program.
    /// - colors 16 to 231 contain several palettes of bright colors,
    ///   arranged in six squares measuring six by six each.
    /// - colors 232 to 255 are shades of grey from black to white.
    ///
    /// It might make more sense to look at a [color chart][cc].
    ///
    /// [cc]: https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg
    Fixed(u8),

    /// A 24-bit Rgb color, as specified by ISO-8613-3.
    Rgb(u8, u8, u8),

    /// The default color (foreground code `39`, background codr `49`).
    #[default]
    Default,
}

macro_rules! color_methods {
    ($($flag:ident),*) => {
        paste! {
            $(
                #[doc = r"Returns a `Style` with the foreground color set to this color, and the `" $flag r"` property turned on."]
                #[doc = r""]
                #[doc = r"# Examples"]
                #[doc = r""]
                #[doc = r"```"]
                #[doc = r"use nu_ansi_term::Color;"]
                #[doc = r""]
                #[doc = r"let style = Color::Yellow." $flag:lower r"();"]
                #[doc = r#"println!("{}", style.paint("hi"));"# ]
                #[doc = r"```"]
                pub fn [< $flag:lower >](self) -> Style {
                    self.foreground().[< $flag:lower >]()
                }
            )*
        }
    };
}

impl Color {
    /// Returns a `Style` with the foreground color set to this color.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color;
    ///
    /// let style = Color::Rgb(31, 31, 31).foreground();
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn foreground(self) -> Style {
        Style::new().foreground(self)
    }

    /// Returns a `Style` with the background set to this color.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color;
    ///
    /// let style = Color::White.background().foreground(Color::Rgb(31, 31, 31));
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn background(self) -> Style {
        Style::new().background(self)
    }

    /// Returns a `Style` with the foreground color set to this color and the
    /// background color property set to the given color.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color;
    ///
    /// let style = Color::Rgb(31, 31, 31).on_background(Color::White);
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn on_background(self, bg: Self) -> Style {
        Style::new().foreground(self).background(bg)
    }

    /// Returns a `Style` with the background color set to this color and the
    /// foreground color property set to the given color.
    pub fn with_fg(self, fg: Self) -> Style {
        Style::new().background(self).foreground(fg)
    }

    color_methods!(
        BOLD,
        DIMMED,
        ITALIC,
        UNDERLINE,
        BLINK,
        REVERSE,
        HIDDEN,
        STRIKETHROUGH
    );
}

impl From<Color> for Style {
    /// You can turn a `Color` into a `Style` with the foreground color set
    /// with the `From` trait.
    ///
    /// ```
    /// use nu_ansi_term::{Style, Color};
    /// let green_foreground = Style::default().foreground(Color::Green);
    /// assert_eq!(green_foreground, Color::Green.foreground());
    /// assert_eq!(green_foreground, Color::Green.into());
    /// assert_eq!(green_foreground, Style::from(Color::Green));
    /// ```
    fn from(color: Color) -> Style {
        color.foreground()
    }
}

#[cfg(test)]
#[cfg(feature = "derive_serde_style")]
mod serde_json_tests {
    use super::{Color, Style};

    #[test]
    fn color_serialization() {
        let colors = &[
            Color::Red,
            Color::Blue,
            Color::Rgb(123, 123, 123),
            Color::Fixed(255),
        ];

        assert_eq!(
            serde_json::to_string(&colors).unwrap(),
            "[\"Red\",\"Blue\",{\"Rgb\":[123,123,123]},{\"Fixed\":255}]"
        );
    }

    #[test]
    fn color_deserialization() {
        let colors = [
            Color::Red,
            Color::Blue,
            Color::Rgb(123, 123, 123),
            Color::Fixed(255),
        ];

        for color in colors {
            let serialized = serde_json::to_string(&color).unwrap();
            let deserialized: Color = serde_json::from_str(&serialized).unwrap();

            assert_eq!(color, deserialized);
        }
    }

    #[test]
    fn style_serialization() {
        let style = Style::default();

        assert_eq!(serde_json::to_string(&style).unwrap(), "{\"foreground\":null,\"background\":null,\"is_bold\":false,\"is_dimmed\":false,\"is_italic\":false,\"is_underline\":false,\"is_blink\":false,\"is_reverse\":false,\"is_hidden\":false,\"is_strikethrough\":false,\"prefix_with_reset\":false}".to_string());
    }
}
