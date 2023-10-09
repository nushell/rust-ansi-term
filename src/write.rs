use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::io;

pub type WriteResult<E> = Result<(), E>;

#[macro_export]
macro_rules! write_any_fmt {
    ($w:expr, $($args:tt)*) => {
        $w.write_any_fmt(std::format_args!($($args)*))
    };
}

#[macro_export]
macro_rules! write_any_str {
    ($w:expr, $($args:tt)*) => {
        $($args)*.write_str_to($w)
    };
}

#[macro_export]
macro_rules! write_any {
    ($w:expr, $($args:tt)*) => {
        {
            let c = $crate::write::Content::from($($args)*);
            c.write_to($w)
        }
    };
}

#[macro_export]
macro_rules! coerce_fmt_write {
    ($w:expr) => {{
        let w: &mut dyn fmt::Write = $w;
        w
    }};
}

#[macro_export]
macro_rules! coerce_io_write {
    ($w:expr) => {{
        let w: &mut dyn io::Write = $w;
        w
    }};
}

pub trait AnyWrite {
    type Buf: ?Sized + ToOwned;
    type Error;

    fn write_any_fmt(&mut self, args: fmt::Arguments) -> WriteResult<Self::Error>;

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error>;
}

impl<'a> AnyWrite for dyn fmt::Write + 'a {
    type Buf = str;
    type Error = fmt::Error;

    fn write_any_fmt(&mut self, args: fmt::Arguments) -> WriteResult<Self::Error> {
        fmt::Write::write_fmt(self, args)
    }

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error> {
        fmt::Write::write_str(self, s)
    }
}

impl<'a> AnyWrite for dyn io::Write + 'a {
    type Buf = [u8];
    type Error = io::Error;

    fn write_any_fmt(&mut self, args: fmt::Arguments) -> WriteResult<Self::Error> {
        io::Write::write_fmt(self, args)
    }

    fn write_any_str(&mut self, s: &Self::Buf) -> WriteResult<Self::Error> {
        io::Write::write_all(self, s)
    }
}

pub trait StrLike<'a, W: AnyWrite + ?Sized>
where
    Self: AsRef<W::Buf>,
{
    fn write_str_to(&self, w: &mut W) -> WriteResult<W::Error>;
}

impl<'a, W: AnyWrite + ?Sized, S: ?Sized + ToOwned + AsRef<W::Buf>> StrLike<'a, W> for S {
    fn write_str_to(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self.as_ref())
    }
}

pub enum Content<'a, S: ?Sized + ToOwned> {
    FmtArgs(fmt::Arguments<'a>),
    StrLike(Cow<'a, S>),
}

impl<'a, S: ?Sized + ToOwned> ToString for Content<'a, S>
where
    S: AsRef<str>,
{
    fn to_string(&self) -> String {
        match self {
            Content::FmtArgs(x) => format!("{}", x),
            Content::StrLike(x) => {
                let mut s = String::new();
                <S as StrLike<'a, dyn fmt::Write>>::write_str_to(x, coerce_fmt_write!(&mut s))
                    .unwrap();
                s
            }
        }
    }
}

impl<'a, S: ?Sized + ToOwned> Clone for Content<'a, S> {
    fn clone(&self) -> Self {
        match self {
            Self::FmtArgs(x) => Self::FmtArgs(*x),
            Self::StrLike(x) => Self::StrLike(x.clone()),
        }
    }
}

impl<'a, S: ?Sized + ToOwned> Debug for Content<'a, S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FmtArgs(x) => f.debug_tuple("FmtArgs").field(x).finish(),
            Self::StrLike(x) => f.debug_tuple("StrLike").field(&x.as_ref()).finish(),
        }
    }
}

impl<'a, S: ?Sized + ToOwned> Content<'a, S> {
    pub fn write_to<T: ?Sized + ToOwned, W: AnyWrite<Buf = T> + ?Sized>(
        &self,
        w: &mut W,
    ) -> WriteResult<W::Error>
    where
        S: StrLike<'a, W>,
    {
        match self {
            Content::FmtArgs(args) => w.write_any_fmt(*args),
            Content::StrLike(s) => <S as StrLike<'a, W>>::write_str_to(s, w),
        }
    }
}

#[macro_export]
macro_rules! content_from {
    (|$inp:ident : $inp_ty:ty | -> Content<'a, S> { $conversion:expr } ) => {
        impl<'a, S: ?Sized + ToOwned> From<&'a S> for Content<'a, S> {
            fn from(inp: &'a S) -> Self {
                $conversion(inp)
            }
        }
    };
}

impl<'a, S: ?Sized + ToOwned, T: ?Sized + ToOwned> From<&'a T> for Content<'a, S>
where
    S: Debug,
    T: AsRef<S>,
{
    fn from(s: &'a T) -> Self {
        Content::StrLike(Cow::Borrowed(s.as_ref()))
    }
}

impl<'a, S: ?Sized + ToOwned> From<fmt::Arguments<'a>> for Content<'a, S>
where
    S: Debug,
{
    fn from(args: fmt::Arguments<'a>) -> Self {
        Content::FmtArgs(args)
    }
}

impl<'a> From<String> for Content<'a, str> {
    fn from(s: String) -> Self {
        Content::StrLike(Cow::Owned(s))
    }
}
