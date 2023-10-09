#![allow(missing_docs)]
use crate::difference::StyleDelta;
use crate::style::{Color, Style, StyleFlags};
use crate::write::{AnyWrite, StrLike, WriteResult};
use crate::{fmt_write, write_any_fmt, write_any_str};
use std::fmt;

impl StyleFlags {
    #[cfg(not(feature = "gnu_legacy"))]
    fn as_format_char(self) -> Option<char> {
        match self {
            StyleFlags::BOLD => '1'.into(),
            StyleFlags::DIMMED => '2'.into(),
            StyleFlags::ITALIC => '3'.into(),
            StyleFlags::UNDERLINE => '4'.into(),
            StyleFlags::BLINK => '5'.into(),
            StyleFlags::REVERSE => '7'.into(),
            StyleFlags::HIDDEN => '8'.into(),
            StyleFlags::STRIKETHROUGH => '9'.into(),
            _ => None,
        }
    }

    #[cfg(feature = "gnu_legacy")]
    fn as_format_char(self) -> Option<&'static str> {
        match self {
            StyleFlags::BOLD => "01".into(),
            StyleFlags::DIMMED => "02".into(),
            StyleFlags::ITALIC => "03".into(),
            StyleFlags::UNDERLINE => "04".into(),
            StyleFlags::BLINK => "05".into(),
            StyleFlags::REVERSE => "07".into(),
            StyleFlags::HIDDEN => "08".into(),
            StyleFlags::STRIKETHROUGH => "09".into(),
            _ => None,
        }
    }
}

impl Style {
    /// Write any bytes that go *before* a piece of text to the given writer.
    pub fn write_prefix<W: AnyWrite + ?Sized>(&self, f: &mut W) -> WriteResult<W::Error>
    where
        str: AsRef<W::Buf>,
        W::Buf: ToOwned,
    {
        // If there are actually no styles here, then don’t write *any* codes
        // as the prefix. An empty ANSI code may not affect the terminal
        // output at all, but a user may just want a code-free string.
        if self.is_empty() {
            return Ok(());
        }

        // Prefix everything with reset characters if needed
        if self.is_prefix_with_reset() {
            write_any_str!(f, "\x1B[0m")?
        }

        if self.has_no_styling() {
            return Ok(());
        }

        fn write_front<W: AnyWrite + ?Sized>(
            f: &mut W,
            write_occurred: bool,
        ) -> WriteResult<W::Error>
        where
            str: AsRef<W::Buf>,
            W::Buf: ToOwned,
        {
            if write_occurred {
                write_any_str!(f, ";")
            } else {
                // front of the ANSI escape code sequence
                write_any_str!(f, "\x1B[")
            }
        }

        fn write_code<W: AnyWrite + ?Sized, T, F: Fn(&mut W, T) -> WriteResult<W::Error>>(
            f: &mut W,
            input: Option<T>,
            write_op: F,
            write_occurred: bool,
        ) -> Result<bool, W::Error>
        where
            str: AsRef<W::Buf>,
            W::Buf: ToOwned,
        {
            if let Some(x) = input {
                write_front(f, write_occurred)?;
                write_op(f, x)?;
                Ok(true)
            } else {
                Ok(write_occurred)
            }
        }

        let mut write_occurred = false;
        for (_, flag) in self.iter_formats() {
            write_occurred = write_code(
                f,
                flag.as_format_char(),
                |f, x| write_any_fmt!(f, "{}", x),
                write_occurred,
            )?;
        }

        // The foreground and background colors, if specified, need to be
        // handled specially because the number codes are more complicated.
        // (see `write_background_code` and `write_foreground_code`)
        write_occurred = write_code(
            f,
            self.is_background(),
            |f, x| x.write_background_code(f),
            write_occurred,
        )?;

        write_occurred = write_code(
            f,
            self.is_foreground(),
            |f, x| x.write_foreground_code(f),
            write_occurred,
        )?;

        if write_occurred {
            // All the codes end with an `m`, because reasons.
            write_any_str!(f, "m")?;
        }

        Ok(())
    }

    /// Write any bytes that go *after* a piece of text to the given writer.
    fn write_suffix<W: AnyWrite + ?Sized>(&self, f: &mut W) -> WriteResult<W::Error> {
        if self.is_empty() {
            Ok(())
        } else {
            write_any_fmt!(f, "{}", RESET)
        }
    }
}

/// The code to send to reset all styles and return to `Style::default()`.
pub static RESET: &str = "\x1B[0m";

impl Color {
    fn write_foreground_code<W: AnyWrite + ?Sized>(&self, f: &mut W) -> WriteResult<W::Error>
    where
        str: AsRef<W::Buf>,
    {
        match self {
            Color::Black => write_any_str!(f, "30"),
            Color::Red => write_any_str!(f, "31"),
            Color::Green => write_any_str!(f, "32"),
            Color::Yellow => write_any_str!(f, "33"),
            Color::Blue => write_any_str!(f, "34"),
            Color::Purple => write_any_str!(f, "35"),
            Color::Magenta => write_any_str!(f, "35"),
            Color::Cyan => write_any_str!(f, "36"),
            Color::White => write_any_str!(f, "37"),
            Color::Fixed(num) => write_any_fmt!(f, "38;5;{}", num),
            Color::Rgb(r, g, b) => write_any_fmt!(f, "38;2;{};{};{}", r, g, b),
            Color::Default => write_any_str!(f, "39"),
            Color::DarkGray => write_any_str!(f, "90"),
            Color::LightRed => write_any_str!(f, "91"),
            Color::LightGreen => write_any_str!(f, "92"),
            Color::LightYellow => write_any_str!(f, "93"),
            Color::LightBlue => write_any_str!(f, "94"),
            Color::LightPurple => write_any_str!(f, "95"),
            Color::LightMagenta => write_any_str!(f, "95"),
            Color::LightCyan => write_any_str!(f, "96"),
            Color::LightGray => write_any_str!(f, "97"),
        }
    }

    fn write_background_code<W: AnyWrite + ?Sized>(&self, f: &mut W) -> WriteResult<W::Error>
    where
        str: AsRef<W::Buf>,
    {
        match self {
            Color::Black => write_any_str!(f, "40"),
            Color::Red => write_any_str!(f, "41"),
            Color::Green => write_any_str!(f, "42"),
            Color::Yellow => write_any_str!(f, "43"),
            Color::Blue => write_any_str!(f, "44"),
            Color::Purple => write_any_str!(f, "45"),
            Color::Magenta => write_any_str!(f, "45"),
            Color::Cyan => write_any_str!(f, "46"),
            Color::White => write_any_str!(f, "47"),
            Color::Fixed(num) => write_any_fmt!(f, "48;5;{}", num),
            Color::Rgb(r, g, b) => write_any_fmt!(f, "48;2;{};{};{}", r, g, b),
            Color::Default => write_any_str!(f, "49"),
            Color::DarkGray => write_any_str!(f, "100"),
            Color::LightRed => write_any_str!(f, "101"),
            Color::LightGreen => write_any_str!(f, "102"),
            Color::LightYellow => write_any_str!(f, "103"),
            Color::LightBlue => write_any_str!(f, "104"),
            Color::LightPurple => write_any_str!(f, "105"),
            Color::LightMagenta => write_any_str!(f, "105"),
            Color::LightCyan => write_any_str!(f, "106"),
            Color::LightGray => write_any_str!(f, "107"),
        }
    }
}

/// Like `AnsiString`, but only displays the style prefix.
///
/// This type implements the `Display` trait, meaning it can be written to a
/// `std::fmt` formatting without doing any extra allocation, and written to a
/// string with the `.to_string()` method. For examples, see
/// [`Style::prefix`](struct.Style.html#method.prefix).
#[derive(Clone, Copy, Debug)]
pub struct Prefix(Style);

/// Like `AnsiString`, but only displays the difference between two
/// styles.
///
/// This type implements the `Display` trait, meaning it can be written to a
/// `std::fmt` formatting without doing any extra allocation, and written to a
/// string with the `.to_string()` method. For examples, see
/// [`Style::infix`](struct.Style.html#method.infix).
#[derive(Clone, Copy, Debug)]
pub struct Infix(pub Style, pub Style);

/// Like `AnsiString`, but only displays the style suffix.
///
/// This type implements the `Display` trait, meaning it can be written to a
/// `std::fmt` formatting without doing any extra allocation, and written to a
/// string with the `.to_string()` method. For examples, see
/// [`Style::suffix`](struct.Style.html#method.suffix).
#[derive(Clone, Copy, Debug)]
pub struct Suffix(Style);

impl Style {
    /// The prefix bytes for this style. These are the bytes that tell the
    /// terminal to use a different color or font style.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[cfg(not(feature = "gnu_legacy"))]
    /// # {
    /// use nu_ansi_term::{Style, Color::Blue};
    ///
    /// let style = Style::default().bold();
    /// assert_eq!("\x1b[1m",
    ///            style.prefix().to_string());
    ///
    /// let style = Blue.bold();
    /// assert_eq!("\x1b[1;34m",
    ///            style.prefix().to_string());
    ///
    /// let style = Style::default();
    /// assert_eq!("",
    ///            style.prefix().to_string());
    /// # }
    /// ```
    ///
    /// # Examples with gnu_legacy feature enabled
    /// Styles like bold, underlined, etc. are two-digit now
    ///
    /// ```
    /// # #[cfg(feature = "gnu_legacy")]
    /// # {
    /// use nu_ansi_term::{Style, Color::Blue};
    ///
    /// let style = Style::default().bold();
    /// assert_eq!("\x1b[01m",
    ///            style.prefix().to_string());
    ///
    /// let style = Blue.bold();
    /// assert_eq!("\x1b[01;34m",
    ///            style.prefix().to_string());
    /// # }
    /// ```
    pub const fn prefix(self) -> Prefix {
        Prefix(self)
    }

    /// The infix bytes between this style and `next` style. These are the bytes
    /// that tell the terminal to change the style to `next`. These may include
    /// a reset followed by the next color and style, depending on the two styles.
    ///
    /// # Examples
    /// ```
    /// # #[cfg(not(feature = "gnu_legacy"))]
    /// # {
    /// use nu_ansi_term::{Style, Color::Green};
    ///
    /// let style = Style::default().bold();
    /// assert_eq!("\x1b[32m",
    ///            style.infix(Green.bold()).to_string());
    ///
    /// let style = Green.foreground();
    /// assert_eq!("\x1b[1m",
    ///            style.infix(Green.bold()).to_string());
    ///
    /// let style = Style::default();
    /// assert_eq!("",
    ///            style.infix(style).to_string());
    /// # }
    /// ```
    /// # Examples with gnu_legacy feature enabled
    /// Styles like bold, underlined, etc. are two-digit now
    /// ```
    /// # #[cfg(feature = "gnu_legacy")]
    /// # {
    /// use nu_ansi_term::Color::Green;
    ///
    /// let style = Green.foreground();
    /// assert_eq!("\x1b[01m",
    ///            style.infix(Green.bold()).to_string());
    /// # }
    /// ```
    pub const fn infix(self, next: Style) -> Infix {
        Infix(self, next)
    }

    /// The suffix for this style. These are the bytes that tell the terminal
    /// to reset back to its normal color and font style.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::{Style, Color::Green};
    ///
    /// let style = Style::default().bold();
    /// assert_eq!("\x1b[0m",
    ///            style.suffix().to_string());
    ///
    /// let style = Green.foreground().bold();
    /// assert_eq!("\x1b[0m",
    ///            style.suffix().to_string());
    ///
    /// let style = Style::default();
    /// assert_eq!("",
    ///            style.suffix().to_string());
    /// ```
    pub const fn suffix(self) -> Suffix {
        Suffix(self)
    }
}

impl Color {
    /// The prefix bytes for this color as a `Style`. These are the bytes
    /// that tell the terminal to use a different color or font style.
    ///
    /// See also [`Style::prefix`](struct.Style.html#method.prefix).
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color::Green;
    ///
    /// assert_eq!("\x1b[32m",
    ///            Green.prefix().to_string());
    /// ```
    pub fn prefix(self) -> Prefix {
        Prefix(self.foreground())
    }

    /// The infix bytes between this color and `next` color. These are the bytes
    /// that tell the terminal to use the `next` color, or to do nothing if
    /// the two colors are equal.
    ///
    /// See also [`Style::infix`](struct.Style.html#method.infix).
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color::{Red, Yellow};
    ///
    /// assert_eq!("\x1b[33m",
    ///            Red.infix(Yellow).to_string());
    /// ```
    pub fn infix(self, next: Color) -> Infix {
        Infix(self.foreground(), next.foreground())
    }

    /// The suffix for this color as a `Style`. These are the bytes that
    /// tell the terminal to reset back to its normal color and font style.
    ///
    /// See also [`Style::suffix`](struct.Style.html#method.suffix).
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::Color::Purple;
    ///
    /// assert_eq!("\x1b[0m",
    ///            Purple.suffix().to_string());
    /// ```
    pub fn suffix(self) -> Suffix {
        Suffix(self.foreground())
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.write_prefix(fmt_write!(f))
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.compute_delta(self.1) {
            StyleDelta::PrefixUsing(style) => style.write_prefix(fmt_write!(f)),
            StyleDelta::Empty => Ok(()),
        }
    }
}

impl fmt::Display for Suffix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.write_suffix(fmt_write!(f))
    }
}

#[macro_export]
macro_rules! create_content_eq_tests {
    () => {};
    (@str_cmp [$test_name:ident: $outcome:expr, $req:literal] $($args:tt)*) => {
        paste! {
            style_test!(
                @str_cmp $test_name:
                try:$outcome;
                req:$req
            );
        }
        create_content_eq_tests!($($args)*);
    };
    ([$test_name:ident: $style:expr, $content:literal, $req:literal] $($args:tt)*) => {
        paste! {
            style_test!(
                @content_eq $test_name:
                try:$style;
                content:$content;
                req:$req
            );
        }
        create_content_eq_tests!($($args)*);
    };
    ([$test_name:ident: $style:expr, $content:expr, $req:literal] $($args:tt)*) => {
        paste! {
            style_test!(
                @content_eq $test_name:
                try:$style;
                content:$content;
                req:$req
            );
        }
        create_content_eq_tests!($($args)*);
    };
}

#[cfg(test)]
#[cfg(not(feature = "gnu_legacy"))]
mod test {
    use crate::create_content_eq_tests;
    use crate::style::Color::*;
    use crate::style::Style;
    use crate::style_test;
    use paste::paste;

    create_content_eq_tests!(
        [plain: Style::default(), "text/plain", "text/plain"]
        [red: Red, "hi", "\x1B[31mhi\x1B[0m"]
        [black: Black.foreground(), "hi", "\x1B[30mhi\x1B[0m"]
        [yellow: Yellow.bold(), "hi", "\x1B[1;33mhi\x1B[0m"]
        [yellow_bold_2: Yellow.foreground().bold(), "hi", "\x1B[1;33mhi\x1B[0m"]
        [blue_underline: Blue.underline(), "hi", "\x1B[4;34mhi\x1B[0m"]
        [green_bold_ul: Green.bold().underline(), "hi", "\x1B[1;4;32mhi\x1B[0m"]
        [green_bold_ul_2: Green.underline().bold(), "hi", "\x1B[1;4;32mhi\x1B[0m"]
        [purple_on_white: Purple.on_background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [purple_on_white_2: Purple.foreground().background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [yellow_on_blue: Style::new().background(Blue).foreground(Yellow), "hi", "\x1B[44;33mhi\x1B[0m"]
        [magenta_on_white: Magenta.on_background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [magenta_on_white_2: Magenta.foreground().background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [yellow_on_blue_2: Cyan.on_background(Blue).foreground(Yellow), "hi", "\x1B[44;33mhi\x1B[0m"]
        [yellow_on_blue_reset: Cyan.on_background(Blue).prefix_with_reset().foreground(Yellow), "hi", "\x1B[0m\x1B[44;33mhi\x1B[0m"]
        [yellow_on_blue_reset_2: Cyan.on_background(Blue).foreground(Yellow).prefix_with_reset(), "hi", "\x1B[0m\x1B[44;33mhi\x1B[0m"]
        [cyan_bold_on_white: Cyan.bold().background(White), "hi", "\x1B[1;47;36mhi\x1B[0m"]
        [cyan_ul_on_white: Cyan.underline().background(White), "hi", "\x1B[4;47;36mhi\x1B[0m"]
        [cyan_bold_ul_on_white: Cyan.bold().underline().background(White), "hi", "\x1B[1;4;47;36mhi\x1B[0m"]
        [cyan_ul_bold_on_white: Cyan.underline().bold().background(White), "hi", "\x1B[1;4;47;36mhi\x1B[0m"]
        [fixed: Fixed(100), "hi", "\x1B[38;5;100mhi\x1B[0m"]
        [fixed_on_purple: Fixed(100).on_background(Purple), "hi", "\x1B[45;38;5;100mhi\x1B[0m"]
        [fixed_on_fixed: Fixed(100).on_background(Fixed(200)), "hi", "\x1B[48;5;200;38;5;100mhi\x1B[0m"]
        [rgb: Rgb(70,130,180), "hi", "\x1B[38;2;70;130;180mhi\x1B[0m"]
        [rgb_on_blue: Rgb(70,130,180).on_background(Blue), "hi", "\x1B[44;38;2;70;130;180mhi\x1B[0m"]
        [blue_on_rgb: Blue.on_background(Rgb(70,130,180)), "hi", "\x1B[48;2;70;130;180;34mhi\x1B[0m"]
        [rgb_on_rgb: Rgb(70,130,180).on_background(Rgb(5,10,15)), "hi", "\x1B[48;2;5;10;15;38;2;70;130;180mhi\x1B[0m"]
        [bold: Style::new().bold(), "hi", "\x1B[1mhi\x1B[0m"]
        [bold_with_reset: Style::new().prefix_with_reset().bold(), "hi", "\x1B[0m\x1B[1mhi\x1B[0m"]
        [bold_with_reset_2: Style::new().bold().prefix_with_reset(), "hi", "\x1B[0m\x1B[1mhi\x1B[0m"]
        [underline: Style::new().underline(), "hi", "\x1B[4mhi\x1B[0m"]
        [bunderline: Style::new().bold().underline(), "hi", "\x1B[1;4mhi\x1B[0m"]
        [dimmed: Style::new().dimmed(), "hi", "\x1B[2mhi\x1B[0m"]
        [italic: Style::new().italic(), "hi", "\x1B[3mhi\x1B[0m"]
        [blink: Style::new().blink(), "hi", "\x1B[5mhi\x1B[0m"]
        [reverse: Style::new().reverse(), "hi", "\x1B[7mhi\x1B[0m"]
        [hidden: Style::new().hidden(), "hi", "\x1B[8mhi\x1B[0m"]
        [stricken: Style::new().strikethrough(), "hi", "\x1B[9mhi\x1B[0m"]
        [lr_on_lr: LightRed.on_background(LightRed), "hi", "\x1B[101;91mhi\x1B[0m"]
        @str_cmp [reset_format: Style::new().dimmed().infix(Style::new()).to_string(), "\x1B[0m"]
        @str_cmp [reset_then_style: White.dimmed().infix(White.foreground()).to_string(), "\x1B[0m\x1B[37m"]
        @str_cmp [color_then_format: White.foreground().infix(White.bold()).to_string(), "\x1B[1m"]
        @str_cmp [color_change: White.foreground().infix(Blue.foreground()).to_string(), "\x1B[34m"]
        @str_cmp [no_change: Blue.bold().infix(Blue.bold()).to_string(), ""]
    );
}

#[cfg(test)]
#[cfg(feature = "gnu_legacy")]
mod gnu_legacy_test {
    use crate::create_content_eq_tests;
    use crate::style::Color::*;
    use crate::style::Style;
    use crate::style_test;
    use paste::paste;

    macro_rules! create_content_eq_tests {
        () => {};
        ([$test_name:ident: $style:expr, $content:expr, $req:literal] $($args:tt)*) => {
            paste! {
                style_test!(
                    @content_eq $test_name:
                    try:$style;
                    content:$content;
                    req:$req
                );
            }
            create_content_eq_tests!($($args)*);
        };
    }

    create_content_eq_tests!(
        [plain: Style::default(), "text/plain", "text/plain"]
        [red: Red, "hi", "\x1B[31mhi\x1B[0m"]
        [black: Black.foreground(), "hi", "\x1B[30mhi\x1B[0m"]
        [yellow_bold: Yellow.bold(), "hi", "\x1B[01;33mhi\x1B[0m"]
        [yellow_bold_2: Yellow.foreground().bold(), "hi", "\x1B[01;33mhi\x1B[0m"]
        [blue_underline: Blue.underline(), "hi", "\x1B[04;34mhi\x1B[0m"]
        [green_bold_ul: Green.bold().underline(), "hi", "\x1B[01;04;32mhi\x1B[0m"]
        [green_bold_ul_2: Green.underline().bold(), "hi", "\x1B[01;04;32mhi\x1B[0m"]
        [purple_on_white: Purple.on_background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [purple_on_white_2: Purple.foreground().background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [yellow_on_blue: Style::new().background(Blue).foreground(Yellow), "hi", "\x1B[44;33mhi\x1B[0m"]
        [yellow_on_blue_reset: Cyan.on_background(Blue).prefix_with_reset().foreground(Yellow), "hi", "\x1B[0m\x1B[44;33mhi\x1B[0m"]
        [yellow_on_blue_reset_2: Cyan.on_background(Blue).foreground(Yellow).prefix_with_reset(), "hi", "\x1B[0m\x1B[44;33mhi\x1B[0m"]
        [magenta_on_white: Magenta.on_background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [magenta_on_white_2: Magenta.foreground().background(White), "hi", "\x1B[47;35mhi\x1B[0m"]
        [yellow_on_blue_2: Cyan.on_background(Blue).foreground(Yellow), "hi", "\x1B[44;33mhi\x1B[0m"]
        [cyan_bold_on_white: Cyan.bold().background(White), "hi", "\x1B[01;47;36mhi\x1B[0m"]
        [cyan_ul_on_white: Cyan.underline().background(White), "hi", "\x1B[04;47;36mhi\x1B[0m"]
        [cyan_bold_ul_on_white: Cyan.bold().underline().background(White), "hi", "\x1B[01;04;47;36mhi\x1B[0m"]
        [cyan_ul_bold_on_white: Cyan.underline().bold().background(White), "hi", "\x1B[01;04;47;36mhi\x1B[0m"]
        [fixed: Fixed(100), "hi", "\x1B[38;5;100mhi\x1B[0m"]
        [fixed_on_purple: Fixed(100).on_background(Purple), "hi", "\x1B[45;38;5;100mhi\x1B[0m"]
        [fixed_on_fixed: Fixed(100).on_background(Fixed(200)), "hi", "\x1B[48;5;200;38;5;100mhi\x1B[0m"]
        [rgb: Rgb(70,130,180), "hi", "\x1B[38;2;70;130;180mhi\x1B[0m"]
        [rgb_on_blue: Rgb(70,130,180).on_background(Blue), "hi", "\x1B[44;38;2;70;130;180mhi\x1B[0m"]
        [blue_on_rgb: Blue.on_background(Rgb(70,130,180)), "hi", "\x1B[48;2;70;130;180;34mhi\x1B[0m"]
        [rgb_on_rgb: Rgb(70,130,180).on_background(Rgb(5,10,15)), "hi", "\x1B[48;2;5;10;15;38;2;70;130;180mhi\x1B[0m"]
        [bold: Style::new().bold(), "hi", "\x1B[01mhi\x1B[0m"]
        [bold_with_reset: Style::new().prefix_with_reset().bold(), "hi", "\x1B[0m\x1B[01mhi\x1B[0m"]
        [bold_with_reset_2: Style::new().bold().prefix_with_reset(), "hi", "\x1B[0m\x1B[01mhi\x1B[0m"]
        [underline: Style::new().underline(), "hi", "\x1B[04mhi\x1B[0m"]
        [bunderline: Style::new().bold().underline(), "hi", "\x1B[01;04mhi\x1B[0m"]
        [dimmed: Style::new().dimmed(), "hi", "\x1B[02mhi\x1B[0m"]
        [italic: Style::new().italic(), "hi", "\x1B[03mhi\x1B[0m"]
        [blink: Style::new().blink(), "hi", "\x1B[05mhi\x1B[0m"]
        [reverse: Style::new().reverse(), "hi", "\x1B[07mhi\x1B[0m"]
        [hidden: Style::new().hidden(), "hi", "\x1B[08mhi\x1B[0m"]
        [stricken: Style::new().strikethrough(), "hi", "\x1B[09mhi\x1B[0m"]
        [lr_on_lr: LightRed.on_background(LightRed), "hi", "\x1B[101;91mhi\x1B[0m"]
    );
}
