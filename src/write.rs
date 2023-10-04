use std::borrow::Cow;
use std::fmt;
use std::io;

pub type WriteResult<E> = Result<(), E>;

pub trait AnyWrite {
    type Buf: ?Sized;
    type Error;

    fn write_any_fmt(&mut self, fmt: fmt::Arguments) -> WriteResult<Self::Error>;

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error>;
}

impl<'a> AnyWrite for dyn fmt::Write + 'a {
    type Buf = str;
    type Error = fmt::Error;

    fn write_any_fmt(&mut self, fmt: fmt::Arguments) -> WriteResult<Self::Error> {
        fmt::Write::write_fmt(self, fmt)
    }

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error> {
        fmt::Write::write_str(self, s)
    }
}

impl<'a> AnyWrite for dyn io::Write + 'a {
    type Buf = [u8];
    type Error = io::Error;

    fn write_any_fmt(&mut self, fmt: fmt::Arguments) -> WriteResult<Self::Error> {
        io::Write::write_fmt(self, fmt)
    }

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error> {
        io::Write::write_all(self, s)
    }
}

pub trait Content<'a, S: 'a + ToOwned + ?Sized> {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error>;
}

impl<'a, S: 'a + ToOwned + ?Sized> Content<'a, S> for str
where
    str: AsRef<S>,
{
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self.as_ref())
    }
}

impl<'a> Content<'a, [u8]> for [u8] {
    fn write_to<W: AnyWrite<Buf = [u8]> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized, C: ?Sized + Content<'a, S>> Content<'a, S> for &'a C {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        <C as Content<'a, S>>::write_to(self, w)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized, C: ?Sized + Content<'a, S> + ToOwned> Content<'a, S>
    for Cow<'a, C>
{
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        <C as Content<'a, S>>::write_to(self.as_ref(), w)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> Content<'a, S> for fmt::Arguments<'a> {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_fmt(*self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> Content<'a, S> for String
where
    str: Content<'a, S>,
{
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        <str as Content<'a, S>>::write_to(self.as_str(), w)
    }
}

pub trait IntoContent<'a, S: 'a + ToOwned + ?Sized, C: Content<'a, S>> {
    fn into_content(self) -> C;
}

impl<'a, S: 'a + ToOwned + ?Sized, C: Content<'a, S>> IntoContent<'a, S, C> for C {
    fn into_content(self) -> C {
        self
    }
}

#[macro_export]
macro_rules! write_any_fmt {
    ($w:expr, $($args:tt)*) => {
        $w.write_any_fmt(std::format_args!($($args)*))
    };
}

#[macro_export]
macro_rules! write_any_str {
    ($w:expr, $($args:tt)*) => {
        $w.write_any_str($($args)*.as_ref())
    };
}

#[macro_export]
macro_rules! write_any {
    ($w:expr, $($args:tt)*) => {
        $($args)*.write_to($w)
    };
}
