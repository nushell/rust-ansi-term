use crate::ansi::{HYPERLINK_RESET, RESET};
use crate::difference::Difference;
use crate::style::{Color, OSControl, Style};
use crate::write::AnyWrite;
use std::borrow::Cow;
use std::fmt;
use std::io;

/// An `AnsiGenericString` includes a generic string type and a `Style` to
/// display that string.  `AnsiString` and `AnsiByteString` are aliases for
/// this type on `str` and `\[u8]`, respectively.
#[derive(Eq, PartialEq, Debug)]
pub struct AnsiGenericString<'a, S: 'a + ToOwned + ?Sized>
where
    <S as ToOwned>::Owned: fmt::Debug,
{
    pub(crate) style: Style,
    pub(crate) string: Cow<'a, S>,
    pub(crate) params: Option<Cow<'a, S>>,
}

/// Cloning an `AnsiGenericString` will clone its underlying string.
///
/// # Examples
///
/// ```
/// use nu_ansi_term::AnsiString;
///
/// let plain_string = AnsiString::from("a plain string");
/// let clone_string = plain_string.clone();
/// assert_eq!(clone_string, plain_string);
/// ```
impl<'a, S: 'a + ToOwned + ?Sized> Clone for AnsiGenericString<'a, S>
where
    <S as ToOwned>::Owned: fmt::Debug,
{
    fn clone(&self) -> AnsiGenericString<'a, S> {
        AnsiGenericString {
            style: self.style,
            string: self.string.clone(),
            params: self.params.clone(),
        }
    }
}

// You might think that the hand-written Clone impl above is the same as the
// one that gets generated with #[derive]. But it’s not *quite* the same!
//
// `str` is not Clone, and the derived Clone implementation puts a Clone
// constraint on the S type parameter (generated using --pretty=expanded):
//
//                  ↓_________________↓
//     impl <'a, S: ::std::clone::Clone + 'a + ToOwned + ?Sized> ::std::clone::Clone
//     for ANSIGenericString<'a, S> where
//     <S as ToOwned>::Owned: fmt::Debug { ... }
//
// This resulted in compile errors when you tried to derive Clone on a type
// that used it:
//
//     #[derive(PartialEq, Debug, Clone, Default)]
//     pub struct TextCellContents(Vec<AnsiString<'static>>);
//                                 ^^^^^^^^^^^^^^^^^^^^^^^^^
//     error[E0277]: the trait `std::clone::Clone` is not implemented for `str`
//
// The hand-written impl above can ignore that constraint and still compile.

/// An ANSI String is a string coupled with the `Style` to display it
/// in a terminal.
///
/// Although not technically a string itself, it can be turned into
/// one with the `to_string` method.
///
/// # Examples
///
/// ```
/// use nu_ansi_term::AnsiString;
/// use nu_ansi_term::Color::Red;
///
/// let red_string = Red.paint("a red string");
/// println!("{}", red_string);
/// ```
///
/// ```
/// use nu_ansi_term::AnsiString;
///
/// let plain_string = AnsiString::from("a plain string");
/// ```
pub type AnsiString<'a> = AnsiGenericString<'a, str>;

/// An `AnsiByteString` represents a formatted series of bytes.  Use
/// `AnsiByteString` when styling text with an unknown encoding.
pub type AnsiByteString<'a> = AnsiGenericString<'a, [u8]>;

impl<'a, I, S: 'a + ToOwned + ?Sized> From<I> for AnsiGenericString<'a, S>
where
    I: Into<Cow<'a, S>>,
    <S as ToOwned>::Owned: fmt::Debug,
{
    fn from(input: I) -> AnsiGenericString<'a, S> {
        AnsiGenericString {
            string: input.into(),
            style: Style::default(),
            params: None,
        }
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> AnsiGenericString<'a, S>
where
    <S as ToOwned>::Owned: fmt::Debug,
{
    /// Produce an ANSI string that changes the title shown
    /// by the terminal emulator.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::AnsiString;
    /// let title_string = AnsiString::title("My Title");
    /// println!("{}", title_string);
    /// ```
    /// Should produce an empty line but set the terminal title.
    pub fn title<I>(title: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Cow<'a, S>>,
    {
        Self {
            string: title.into(),
            style: Style::title(),
            params: None,
        }
    }

    /// Produce an ANSI string that notifies the terminal
    /// emulator that the running application is better
    /// represented by the icon found at a given path.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::AnsiString;
    /// let icon_string = AnsiString::icon(std::path::Path::new("foo/bar.icn").to_string_lossy());
    /// println!("{}", icon_string);
    /// ```
    /// Should produce an empty line but set the terminal icon.
    /// Notice that we use std::path to be portable.
    pub fn icon<I>(path: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Cow<'a, S>>,
    {
        Self {
            string: path.into(),
            style: Style::icon(),
            params: None,
        }
    }

    /// Produce an ANSI string that notifies the terminal
    /// emulator the current working directory has changed
    /// to the given path.
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::AnsiString;
    /// let cwd_string = AnsiString::cwd(std::path::Path::new("/foo/bar").to_string_lossy());
    /// println!("{}", cwd_string);
    /// ```
    /// Should produce an empty line but inform the terminal emulator
    /// that the current directory is /foo/bar.
    /// Notice that we use std::path to be portable.
    pub fn cwd<I>(path: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Cow<'a, S>>,
    {
        Self {
            string: path.into(),
            style: Style::cwd(),
            params: None,
        }
    }

    /// Cause the styled ANSI string to link to the given URL
    ///
    /// # Examples
    ///
    /// ```
    /// use nu_ansi_term::AnsiString;
    /// use nu_ansi_term::Color::Red;
    ///
    /// let mut link_string = Red.paint("a red string");
    /// link_string.hyperlink("https://www.example.com");
    /// println!("{}", link_string);
    /// ```
    /// Should show a red-painted string which, on terminals
    /// that support it, is a clickable hyperlink.
    pub fn hyperlink<I>(&mut self, url: I)
    where
        I: Into<Cow<'a, S>>,
    {
        self.style.hyperlink();
        self.params = Some(url.into());
    }

    /// Directly access the style
    pub const fn style_ref(&self) -> &Style {
        &self.style
    }

    /// Directly access the style mutably
    pub fn style_ref_mut(&mut self) -> &mut Style {
        &mut self.style
    }

    // Directly access the underlying string
    pub fn as_str(&self) -> &S {
        self.string.as_ref()
    }
}

/// A set of `AnsiGenericStrings`s collected together, in order to be
/// written with a minimum of control characters.
#[derive(Debug, Eq, PartialEq)]
pub struct AnsiGenericStrings<'a, S: 'a + ToOwned + ?Sized>(pub &'a [AnsiGenericString<'a, S>])
where
    <S as ToOwned>::Owned: fmt::Debug,
    S: PartialEq;

/// A set of `AnsiString`s collected together, in order to be written with a
/// minimum of control characters.
pub type AnsiStrings<'a> = AnsiGenericStrings<'a, str>;

/// A function to construct an `AnsiStrings` instance.
#[allow(non_snake_case)]
pub const fn AnsiStrings<'a>(arg: &'a [AnsiString<'a>]) -> AnsiStrings<'a> {
    AnsiGenericStrings(arg)
}

/// A set of `AnsiByteString`s collected together, in order to be
/// written with a minimum of control characters.
pub type AnsiByteStrings<'a> = AnsiGenericStrings<'a, [u8]>;

/// A function to construct an `AnsiByteStrings` instance.
#[allow(non_snake_case)]
pub const fn AnsiByteStrings<'a>(arg: &'a [AnsiByteString<'a>]) -> AnsiByteStrings<'a> {
    AnsiGenericStrings(arg)
}

// ---- paint functions ----

impl Style {
    /// Paints the given text with this color, returning an ANSI string.
    #[must_use]
    pub fn paint<'a, I, S: 'a + ToOwned + ?Sized>(self, input: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Cow<'a, S>>,
        <S as ToOwned>::Owned: fmt::Debug,
    {
        AnsiGenericString {
            string: input.into(),
            style: self,
            params: None,
        }
    }
}

impl Color {
    /// Paints the given text with this color, returning an ANSI string.
    /// This is a short-cut so you don’t have to use `Blue.normal()` just
    /// to get blue text.
    ///
    /// ```
    /// use nu_ansi_term::Color::Blue;
    /// println!("{}", Blue.paint("da ba dee"));
    /// ```
    #[must_use]
    pub fn paint<'a, I, S: 'a + ToOwned + ?Sized>(self, input: I) -> AnsiGenericString<'a, S>
    where
        I: Into<Cow<'a, S>>,
        <S as ToOwned>::Owned: fmt::Debug,
    {
        AnsiGenericString {
            string: input.into(),
            style: self.normal(),
            params: None,
        }
    }
}

// ---- writers for individual ANSI strings ----

impl<'a> fmt::Display for AnsiString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let w: &mut dyn fmt::Write = f;
        self.write_to_any(w)
    }
}

impl<'a> AnsiByteString<'a> {
    /// Write an `AnsiByteString` to an `io::Write`.  This writes the escape
    /// sequences for the associated `Style` around the bytes.
    pub fn write_to<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        let w: &mut dyn io::Write = w;
        self.write_to_any(w)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> AnsiGenericString<'a, S>
where
    <S as ToOwned>::Owned: fmt::Debug,
    &'a S: AsRef<[u8]>,
{
    fn write_to_any<W: AnyWrite<Wstr = S> + ?Sized>(&self, w: &mut W) -> Result<(), W::Error> {
        write!(w, "{}", self.style.prefix())?;
        if let (Some(s), Some(_)) = (&self.params, self.style.oscontrol) {
            w.write_str(s.as_ref())?;
            write!(w, "\x1B\\")?;
        }
        w.write_str(self.string.as_ref())?;
        write!(w, "{}", self.style.suffix())
    }
}

// ---- writers for combined ANSI strings ----

impl<'a> fmt::Display for AnsiStrings<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let f: &mut dyn fmt::Write = f;
        self.write_to_any(f)
    }
}

impl<'a> AnsiByteStrings<'a> {
    /// Write `AnsiByteStrings` to an `io::Write`.  This writes the minimal
    /// escape sequences for the associated `Style`s around each set of
    /// bytes.
    pub fn write_to<W: io::Write>(&self, w: &mut W) -> io::Result<()> {
        let w: &mut dyn io::Write = w;
        self.write_to_any(w)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized + PartialEq> AnsiGenericStrings<'a, S>
where
    <S as ToOwned>::Owned: fmt::Debug,
    &'a S: AsRef<[u8]>,
{
    fn write_to_any<W: AnyWrite<Wstr = S> + ?Sized>(&self, w: &mut W) -> Result<(), W::Error> {
        use self::Difference::*;

        let first = match self.0.first() {
            None => return Ok(()),
            Some(f) => f,
        };

        write!(w, "{}", first.style.prefix())?;
        if let (Some(s), Some(_)) = (&first.params, first.style.oscontrol) {
            w.write_str(s.as_ref())?;
            write!(w, "\x1B\\")?;
        }
        w.write_str(first.string.as_ref())?;

        for window in self.0.windows(2) {
            match Difference::between(&window[0].style, &window[1].style) {
                ExtraStyles(style) => {
                    write!(w, "{}", style.prefix())?;
                    if let (Some(OSControl::Hyperlink), Some(s)) =
                        (style.oscontrol, &window[1].params)
                    {
                        w.write_str(s.as_ref())?;
                        write!(w, "\x1B\\")?;
                    }
                }
                Reset => match (&window[0].style, &window[1].style) {
                    (
                        Style {
                            oscontrol: Some(OSControl::Hyperlink),
                            ..
                        },
                        Style {
                            oscontrol: None, ..
                        },
                    ) => {
                        write!(
                            w,
                            "{}{}{}",
                            HYPERLINK_RESET,
                            RESET,
                            window[1].style.prefix()
                        )?;
                    }
                    (
                        Style {
                            oscontrol: Some(_), ..
                        },
                        Style {
                            oscontrol: None, ..
                        },
                    ) => {
                        write!(w, "\x1B\\{}", window[1].style.prefix())?;
                    }
                    (_, _) => {
                        write!(w, "{}{}", RESET, window[1].style.prefix())?;
                    }
                },
                Empty => { /* Do nothing! */ }
            }

            w.write_str(&window[1].string)?;
        }

        // Write the final reset string after all of the AnsiStrings have been
        // written, *except* if the last one has no styles, because it would
        // have already been written by this point.
        if let Some(last) = self.0.last() {
            if !last.style.is_plain() {
                match last.style.oscontrol {
                    Some(OSControl::Hyperlink) => {
                        write!(w, "{}{}", HYPERLINK_RESET, RESET)?;
                    }
                    Some(_) => {
                        write!(w, "\x1B\\")?;
                    }
                    _ => {
                        write!(w, "{}", RESET)?;
                    }
                }
            }
        }

        Ok(())
    }
}

// ---- tests ----

#[cfg(test)]
mod tests {
    pub use super::super::{AnsiGenericString, AnsiStrings};
    pub use crate::style::Color::*;
    pub use crate::style::Style;

    #[test]
    fn no_control_codes_for_plain() {
        let one = Style::default().paint("one");
        let two = Style::default().paint("two");
        let output = AnsiStrings(&[one, two]).to_string();
        assert_eq!(output, "onetwo");
    }

    // NOTE: unstyled because it could have OSC escape sequences
    fn idempotent(unstyled: AnsiGenericString<'_, str>) {
        let before_g = Green.paint("Before is Green. ");
        let before = Style::default().paint("Before is Plain. ");
        let after_g = Green.paint(" After is Green.");
        let after = Style::default().paint(" After is Plain.");
        let unstyled_s = unstyled.clone().to_string();

        // check that RESET precedes unstyled
        let joined = AnsiStrings(&[before_g.clone(), unstyled.clone()]).to_string();
        assert!(joined.starts_with("\x1B[32mBefore is Green. \x1B[0m"));
        assert!(
            joined.ends_with(unstyled_s.as_str()),
            "{:?} does not end with {:?}",
            joined,
            unstyled_s
        );

        // check that RESET does not follow unstyled when appending styled
        let joined = AnsiStrings(&[unstyled.clone(), after_g.clone()]).to_string();
        assert!(
            joined.starts_with(unstyled_s.as_str()),
            "{:?} does not start with {:?}",
            joined,
            unstyled_s
        );
        assert!(joined.ends_with("\x1B[32m After is Green.\x1B[0m"));

        // does not introduce spurious SGR codes (reset or otherwise) adjacent
        // to plain strings
        let joined = AnsiStrings(&[unstyled.clone()]).to_string();
        assert!(
            !joined.contains("\x1B["),
            "{:?} does contain \\x1B[",
            joined
        );
        let joined = AnsiStrings(&[before.clone(), unstyled.clone()]).to_string();
        assert!(
            !joined.contains("\x1B["),
            "{:?} does contain \\x1B[",
            joined
        );
        let joined = AnsiStrings(&[before.clone(), unstyled.clone(), after.clone()]).to_string();
        assert!(
            !joined.contains("\x1B["),
            "{:?} does contain \\x1B[",
            joined
        );
        let joined = AnsiStrings(&[unstyled.clone(), after.clone()]).to_string();
        assert!(
            !joined.contains("\x1B["),
            "{:?} does contain \\x1B[",
            joined
        );
    }

    #[test]
    fn title() {
        let title = Style::title().paint("Test Title");
        assert_eq!(title.clone().to_string(), "\x1B]2;Test Title\x1B\\");
        idempotent(title)
    }

    #[test]
    fn icon() {
        let icon = Style::icon().paint("/path/to/test.icn");
        assert_eq!(icon.to_string(), "\x1B]I;/path/to/test.icn\x1B\\");
        idempotent(icon)
    }

    #[test]
    fn cwd() {
        let cwd = Style::cwd().paint("/path/to/test/");
        assert_eq!(cwd.to_string(), "\x1B]7;/path/to/test/\x1B\\");
        idempotent(cwd)
    }

    #[test]
    fn hyperlink() {
        let mut styled = Red.paint("Link to example.com.");
        styled.hyperlink("https://example.com");
        assert_eq!(
            styled.to_string(),
            "\x1B[31m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m"
        );
    }

    #[test]
    fn hyperlinks() {
        let before = Green.paint("Before link. ");
        let mut link = Blue.underline().paint("Link to example.com.");
        let after = Green.paint(" After link.");
        link.hyperlink("https://example.com");

        // Assemble with link by itself
        let joined = AnsiStrings(&[link.clone()]).to_string();
        #[cfg(feature = "gnu_legacy")]
        assert_eq!(joined, format!("\x1B[04;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m"));
        #[cfg(not(feature = "gnu_legacy"))]
        assert_eq!(joined, format!("\x1B[4;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m"));

        // Assemble with link in the middle
        let joined = AnsiStrings(&[before.clone(), link.clone(), after.clone()]).to_string();
        #[cfg(feature = "gnu_legacy")]
        assert_eq!(joined, format!("\x1B[32mBefore link. \x1B[04;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m\x1B[32m After link.\x1B[0m"));
        #[cfg(not(feature = "gnu_legacy"))]
        assert_eq!(joined, format!("\x1B[32mBefore link. \x1B[4;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m\x1B[32m After link.\x1B[0m"));

        // Assemble with link first
        let joined = AnsiStrings(&[link.clone(), after.clone()]).to_string();
        #[cfg(feature = "gnu_legacy")]
        assert_eq!(joined, format!("\x1B[04;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m\x1B[32m After link.\x1B[0m"));
        #[cfg(not(feature = "gnu_legacy"))]
        assert_eq!(joined, format!("\x1B[4;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m\x1B[32m After link.\x1B[0m"));

        // Assemble with link at the end
        let joined = AnsiStrings(&[before.clone(), link.clone()]).to_string();
        #[cfg(feature = "gnu_legacy")]
        assert_eq!(joined, format!("\x1B[32mBefore link. \x1B[04;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m"));
        #[cfg(not(feature = "gnu_legacy"))]
        assert_eq!(joined, format!("\x1B[32mBefore link. \x1B[4;34m\x1B]8;;https://example.com\x1B\\Link to example.com.\x1B]8;;\x1B\\\x1B[0m"));
    }
}
