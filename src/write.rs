use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::io;

pub type WriteResult<E> = Result<(), E>;

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

impl<B: AsMut<str> + fmt::Write> AnyWrite for B {
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

pub trait StrLike<'a, S: 'a + ToOwned + ?Sized>
where
    Self: AsRef<S>,
{
    fn write_str_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error>;
}

impl<'a, S: 'a + ToOwned + ?Sized> StrLike<'a, S> for str
where
    Self: AsRef<S>,
{
    fn write_str_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self.as_ref())
    }
}

impl<'a> StrLike<'a, [u8]> for [u8]
where
    Self: AsRef<[u8]>,
{
    fn write_str_to<W: AnyWrite<Buf = [u8]> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self)
    }
}

impl<'a, S: 'a + ?Sized + ToOwned + StrLike<'a, T>, T: 'a + ?Sized + ToOwned> StrLike<'a, T>
    for &'a S
where
    S: AsRef<T>,
{
    fn write_str_to<W: AnyWrite<Buf = T> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
        w.write_any_str(self.as_ref())
    }
}

pub enum Content<'a, S: ?Sized + ToOwned> {
    FmtArgs(fmt::Arguments<'a>),
    StrLike(Cow<'a, S>),
}

impl<'a, S: ?Sized + ToOwned> ToString for Content<'a, S>
where
    S: StrLike<'a, str>,
{
    fn to_string(&self) -> String {
        match self {
            Content::FmtArgs(x) => format!("{}", x),
            Content::StrLike(x) => {
                let mut s = String::new();
                x.as_ref().write_str_to(&mut s).unwrap();
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
    S: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FmtArgs(x) => f.debug_tuple("FmtArgs").field(x).finish(),
            Self::StrLike(x) => f.debug_tuple("StrLike").field(&x.as_ref()).finish(),
        }
    }
}

impl<'a, S: ?Sized + ToOwned> Content<'a, S> {
    pub fn write_to<B: 'a + ToOwned + ?Sized, W: AnyWrite<Buf = B> + ?Sized>(
        &self,
        w: &mut W,
    ) -> WriteResult<W::Error>
    where
        S: StrLike<'a, B>,
    {
        match self {
            Content::FmtArgs(args) => w.write_any_fmt(*args),
            Content::StrLike(s) => s.write_str_to(w),
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
    S::Owned: Debug,
    T: AsRef<S>,
{
    fn from(s: &'a T) -> Self {
        Content::StrLike(Cow::Borrowed(s.as_ref()))
    }
}

impl<'a, S: ?Sized + ToOwned> From<fmt::Arguments<'a>> for Content<'a, S>
where
    S::Owned: Debug,
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

// pub trait IntoContent<'a, S: 'a + ToOwned + ?Sized> {
//     fn into_content(&self) -> Content<'a, S>;
// }

// impl<'a, S: 'a + ToOwned + ?Sized> IntoContent<'a, S> for str
// where
//
// {
//     #[inline]
//     fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         self.as_ref()
//     }
// }

// impl<'a> IntoContent<'a, [u8]> for [u8] {
//     fn write_to<W: AnyWrite<Buf = [u8]> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         w.write_any_str(self)
//     }
// }

// impl<'a, S: 'a + ToOwned + ?Sized, C: ?Sized + IntoContent<'a, S>> IntoContent<'a, S> for &'a C {
//     fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         <C as IntoContent<'a, S>>::write_to(self, w)
//     }
// }

// impl<'a, S: 'a + ToOwned + ?Sized, C: ?Sized + IntoContent<'a, S> + ToOwned> IntoContent<'a, S>
//     for Cow<'a>
// {
//     fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         <C as IntoContent<'a, S>>::write_to(self.as_ref(), w)
//     }
// }

// impl<'a, S: 'a + ToOwned + ?Sized> IntoContent<'a, S> for fmt::Arguments<'a> {
//     fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         w.write_any_fmt(*self)
//     }
// }

// writeable_via_str!(String, |s: String| s.as_str());

// impl<'a, S: 'a + ToOwned + ?Sized> IntoContent<'a, S> for String
// where
//     str: StrLike<'a, S>,
// {
//     fn into_content(&self) -> Content<'a, S> {}
// }

// impl<'a, S: 'a + ToOwned + ?Sized> IntoContent<'a, S> for PathBuf
// where
//     str: IntoContent<'a, S>,
// {
//     fn write_to<W: AnyWrite<Buf = S> + ?Sized>(&self, w: &mut W) -> WriteResult<W::Error> {
//         <str as IntoContent<'a, S>>::write_to(self.as_str(), w)
//     }
// }

// pub trait IntoWriteable<'a, S: 'a + ToOwned + ?Sized> {
//     type Writeable: IntoContent<'a, S>;
//     fn into_content(self) -> Self::Writeable;
// }

// impl<'a, S: 'a + ToOwned + ?Sized, C: IntoContent<'a, S>> IntoWriteable<'a, S> for C {
//     type Writeable = C;
//     fn into_content(self) -> Self::Writeable {
//         self
//     }
// }

// impl<'a, S> IntoWriteable<'a, S> for Content<'a, S> {}

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
