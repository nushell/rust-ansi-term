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

pub trait IntoWriteable<C> {
    fn into_content(self) -> C;
}

pub trait Writeable<'a, S: 'a + ToOwned + ?Sized> {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error>;
}

impl<'a, S: 'a + ToOwned + ?Sized> Writeable<'a, S> for &'a S {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> Writeable<'a, S> for Cow<'a, S> {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> Writeable<'a, S> for fmt::Arguments<'a> {
    fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_fmt(*self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> IntoWriteable<Cow<'a, S>> for &'a S {
    fn into_content(self) -> Cow<'a, S> {
        Cow::Borrowed(self)
    }
}

impl<'a, S: 'a + ToOwned + ?Sized> IntoWriteable<Cow<'a, S>> for Cow<'a, S> {
    fn into_content(self) -> Cow<'a, S> {
        self.clone()
    }
}

impl<'a> IntoWriteable<fmt::Arguments<'a>> for fmt::Arguments<'a> {
    fn into_content(self) -> Self {
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
macro_rules! write_any_content {
    ($w:expr, $($args:tt)*) => {
        $($args)*.write_to($w)
    };
}
