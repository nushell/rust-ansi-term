use std::hash::Hash;

use bitflags::bitflags;
use paste::paste;

bitflags! {
    #[derive(Clone, Copy, Default, PartialEq, Eq)]
    pub struct FormatFlags: u16 {
        /// Whether this style is bold.
        const BOLD = 1 << 1;
        /// Whether this style is dimmed.
        const DIMMED = 1 << 2;
        /// Whether this style is italic.
        const ITALIC = 1 << 3;
        /// Whether this style is underlined.
        const UNDERLINE = 1 << 4;
        /// Whether this style is blinking.
        const BLINK = 1 << 5;
        /// Whether this style has reverse colors.
        const REVERSE = 1 << 6;
        /// Whether this style is hidden.
        const HIDDEN = 1 << 7;
        /// Whether this style is struckthrough.
        const STRIKETHROUGH = 1 << 8;
    }
}

impl FormatFlags {
    #[inline]
    pub fn set_flags(mut self, flags: FormatFlags) -> Self {
        self.insert(flags);
        self
    }

    #[inline]
    pub fn unset_flags(mut self, flags: FormatFlags) -> Self {
        self &= !flags;
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct Coloring {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
}

impl Coloring {
    /// Check if there are no colors set.
    pub const fn is_empty(&self) -> bool {
        self.fg.is_none() && self.bg.is_none()
    }
}

pub trait BasedOn {
    fn rebase_on(self, base: Self) -> Self;
}

impl BasedOn for Option<Color> {
    fn rebase_on(self, base: Self) -> Self {
        match (self, base) {
            (None, None) => None,
            (None, Some(c)) => Some(c),
            (Some(c), None) | (Some(c), Some(_)) => Some(c),
        }
    }
}

impl BasedOn for Coloring {
    fn rebase_on(self, base: Self) -> Self {
        Self {
            fg: self.fg.rebase_on(base.fg),
            bg: self.bg.rebase_on(base.bg),
        }
    }
}

impl BasedOn for FormatFlags {
    fn rebase_on(self, base: Self) -> Self {
        self | base
    }
}

impl BasedOn for bool {
    fn rebase_on(self, base: Self) -> Self {
        self || base
    }
}

/// A style is a collection of properties that can format a string
/// using ANSI escape codes.
///
/// # Examples
///
/// ```
/// use nu_ansi_term::{Style, Color};
///
/// let style = Style::new().bold().bg(Color::Black);
/// println!("{}", style.paint("Bold on black"));
/// ```
#[derive(Clone, Copy)]
#[cfg_attr(
    feature = "derive_serde_style",
    derive(serde::Deserialize, serde::Serialize)
)]
pub struct Style {
    /// Whether this style will be prefixed with [`RESET`](crate::ansi::RESET).
    pub reset_before_style: bool,
    /// Flags representing whether particular formatting properties are set or not.
    pub formats: FormatFlags,
    /// Data regarding the foreground/background color applied by this style.
    pub coloring: Coloring,
}

impl BasedOn for Style {
    fn rebase_on(self, base: Self) -> Self {
        Style {
            reset_before_style: self.reset_before_style.rebase_on(base.reset_before_style),
            formats: self.formats.rebase_on(base.formats),
            coloring: self.coloring.rebase_on(base.coloring),
        }
    }
}

impl PartialEq for Style {
    fn eq(&self, other: &Self) -> bool {
        self.formats.symmetric_difference(other.formats).is_empty()
            && self.is_fg() == other.is_fg()
            && self.is_bg() == other.is_bg()
    }
}

impl Eq for Style {}

impl Default for Style {
    /// Returns a style with *no* properties set. Formatting text using this
    /// style returns the exact same text.
    ///
    /// ```
    /// use nu_ansi_term::Style;
    /// assert_eq!(None,  Style::default().is_fg());
    /// assert_eq!(None,  Style::default().is_bg());
    /// assert_eq!(false, Style::default().is_bold());
    /// assert_eq!("txt", Style::default().paint("txt").to_string());
    /// ```
    fn default() -> Self {
        Style {
            reset_before_style: false,
            formats: FormatFlags::empty(),
            coloring: Coloring::default(),
        }
    }
}

macro_rules! format_methods {
    ($flag:ident) => {
        paste! {
            #[doc = r"Returns a copy of this style with the [`FormatFlags::`" $flag r"`] property set"]
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
                (*self).insert_formats(FormatFlags::$flag)
            }

            #[doc = r"Checks if the [`FormatFlags::`" $flag r"`] property is set."]
            pub const fn [< is_ $flag:lower >](&self) -> bool {
                self.formats.contains(FormatFlags::$flag)
            }

            #[doc = r"Returns a copy of this style with the [`FormatFlags::`" $flag r"`] property unset."]
            pub fn [< without_ $flag:lower >](&self) -> Style {
                (*self).remove_formats(FormatFlags::$flag)
            }
        }
    }
}

macro_rules! style_color_methods {
    ($ground:ident) => {
        paste! {
            #[doc = r"Sets the color of this style to the provided color option (possibly `None`), then returns it."]
            #[doc = r""]
            #[doc = r"# Examples"]
            #[doc = r""]
            #[doc = r"```"]
            #[doc = r"use nu_ansi_term::Style;"]
            #[doc = r"use nu_ansi_term::Color;"]
            #[doc = r""]
            #[doc = r"let style = Style::new()." [< $ground >] r"(Color::Red);"]
            #[doc = r#"println!("{}", style.paint("hey"));"# ]
            #[doc = r"```"]
            #[inline]
            pub fn [< set_ $ground >](mut self, color: Option<Color>) -> Self {
                self.coloring.[< $ground >] = color;
                self
            }

            #[doc = r"Set the " $ground " color of the style."]
            #[inline]
            pub fn [< $ground >](self, color: Color) -> Self {
                self.[< set_ $ground >](color.into())
            }

            #[doc = r"Gets the corresponding " $ground " color if it exists."]
            #[inline]
            pub const fn [< is_ $ground >](&self) -> Option<Color> {
                self.coloring.[< $ground >]
            }
        }
    };
}

macro_rules! create_format_methods {
    ($($flag:ident),*) => {
        $(format_methods!($flag);)*
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

    /// Insert (turn on) style properties in this style that are true in given `formats`.
    pub fn insert_formats(mut self, formats: FormatFlags) -> Self {
        self.formats.insert(formats);
        self
    }

    /// Remove (turn off) the format properties specified by `formats`.
    pub fn remove_formats(mut self, formats: FormatFlags) -> Self {
        // We use &! instead of the `remove` operator on `flags`, because !
        // truncates any unknown bits.
        self.formats &= !formats;
        self
    }

    /// Create a copy of this style, and insert into it any formats
    /// that are true in `flags`.
    #[inline]
    pub fn with_flags(&self, flags: FormatFlags) -> Style {
        (*self).insert_formats(flags)
    }

    /// Create a copy of this style, and remove from it any formats that are
    /// true in `flags`.
    #[inline]
    pub fn without_flags(&self, flags: FormatFlags) -> Style {
        (*self).remove_formats(flags)
    }

    create_format_methods!(
        BOLD,
        DIMMED,
        ITALIC,
        UNDERLINE,
        BLINK,
        REVERSE,
        HIDDEN,
        STRIKETHROUGH
    );

    style_color_methods!(fg);
    style_color_methods!(bg);

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
        self.formats.is_empty() && self.coloring.is_empty() && !self.reset_before_style
    }

    /// Check if style has no formatting or coloring (it might still have `reset_before_style`).
    #[inline]
    pub fn has_no_styling(&self) -> bool {
        !self.has_color() && !self.has_formatting()
    }

    /// Check if style has any coloring.
    #[inline]
    pub fn has_color(&self) -> bool {
        self.coloring.fg.is_some() || self.coloring.bg.is_some()
    }

    /// Get the formatting flags of this style.
    #[inline]
    pub fn get_formats(&self) -> FormatFlags {
        self.formats
    }

    /// Check if the style contains some property which cannot be inverted, and
    /// thus must be followed by a reset flag in order turn off its effect.
    #[inline]
    pub fn has_formatting(&self) -> bool {
        !self.formats.is_empty()
    }

    /// Create a copy of this style with the specified `coloring`.
    pub fn coloring(self, coloring: Coloring) -> Self {
        self.set_fg(coloring.fg).set_bg(coloring.bg)
    }

    /// Create a copy of this style, with the styling properties updated using
    pub fn update_with(self, other: Self) -> Self {
        Self {
            reset_before_style: !self.reset_before_style && other.reset_before_style,
            formats: self.formats.set_flags(other.formats),
            coloring: Coloring {
                fg: if self.coloring.fg.is_none() {
                    other.coloring.fg
                } else {
                    self.coloring.fg
                },
                bg: if self.coloring.bg.is_none() {
                    other.coloring.bg
                } else {
                    self.coloring.bg
                },
            },
        }
    }

    /// Return whether or not `reset_before_style` is set.
    pub fn is_prefix_with_reset(&self) -> bool {
        self.reset_before_style
    }

    /// Set `reset_before_style` to be `true`.
    pub fn reset_before_style(mut self) -> Self {
        self.reset_before_style = true;
        self
    }

    ///Set `reset_before_style` to the specified value.
    pub fn set_prefix_with_reset(mut self, value: bool) -> Self {
        self.reset_before_style = value;
        self
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
                    self.normal().[< $flag:lower >]()
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
    /// let style = Color::Rgb(31, 31, 31).normal();
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn normal(self) -> Style {
        Style::new().fg(self)
    }

    /// Returns a `Style` with the background set to this color.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color;
    ///
    /// let style = Color::White.bg().fg(Color::Rgb(31, 31, 31));
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn bg(self) -> Style {
        Style::new().bg(self)
    }

    /// Returns a `Style` with the foreground color set to this color and the
    /// background color property set to the given color.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color;
    ///
    /// let style = Color::Rgb(31, 31, 31).on(Color::White);
    /// println!("{}", style.paint("eyyyy"));
    /// ```
    pub fn on(self, bg: Self) -> Style {
        Style::new().fg(self).bg(bg)
    }

    /// Returns a `Style` with the background color set to this color and the
    /// foreground color property set to the given color.
    pub fn under(self, fg: Self) -> Style {
        Style::new().bg(self).fg(fg)
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
    /// let green_foreground = Style::default().fg(Color::Green);
    /// assert_eq!(green_foreground, Color::Green.normal());
    /// assert_eq!(green_foreground, Color::Green.into());
    /// assert_eq!(green_foreground, Style::from(Color::Green));
    /// ```
    fn from(color: Color) -> Style {
        color.normal()
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

        assert_eq!(serde_json::to_string(&style).unwrap(), "{\"foreground\":null,\"background\":null,\"is_bold\":false,\"is_dimmed\":false,\"is_italic\":false,\"is_underline\":false,\"is_blink\":false,\"is_reverse\":false,\"is_hidden\":false,\"is_strikethrough\":false,\"reset_before_style\":false}".to_string());
    }
}
