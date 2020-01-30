
// I think that this is, in general, a pretty messy parser. If I were to clean
// it up, I would probably rewrite it to use nom. Rust syntax and macros just
// aren't that suited to the monadic parsing style, or at least not the way
// chomp is currently implemented.

use chomp::prelude::*;
use chomp::types::{Buffer};
use chomp::ascii::{is_alphanumeric, is_digit};
use chomp::parsers::Error as ChompError;

use crate::camlib::{CommonBind};

pub enum Error {
    Utf8Error(std::string::FromUtf8Error),
    BadNumberError(std::num::ParseIntError),
    ParseFailure(ChompError<u8>),
}

impl From<ChompError<u8>> for Error {
    fn from(e: ChompError<u8>) -> Self {
        Error::ParseFailure(e)
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(e: std::string::FromUtf8Error) -> Self {
        Error::Utf8Error(e)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::BadNumberError(e)
    }
}

pub type ParseResult<I, T> = chomp::prelude::ParseResult<I, T, Error>;

fn is_hexdigit(c: u8) -> bool {
    match c {
        b'0'..=b'9' => true,
        b'a'..=b'f' => true,
        b'A'..=b'F' => true,
        _ => false,
    }
}

fn hex<I:U8Input>(i:I) -> ParseResult<I, u32> {
    or(i,
        |i| token(i, b'$').map(|_|()),
        |i| string(i,b"0x").map(|_|())).then(|i|
    take_while1(i,is_hexdigit).bind(|i,v| {
        let v : Result<_, Error> =
            String::from_utf8(v.into_vec())
            .common_bind(|v| u32::from_str_radix(&v, 16));
        match v {
            Ok(v) => i.ret(v),
            Err(e) => i.err(e)
        }
    }))
}

fn dec<I:U8Input>(i:I) -> ParseResult<I, u32> {
    take_while1(i, is_digit).bind(|i,v| {
        let v : Result<_, Error> =
            String::from_utf8(v.into_vec())
            .common_bind(|v| u32::from_str_radix(&v, 10));
        match v {
            Ok(v) => i.ret(v),
            Err(e) => i.err(e)
        }
    })
}

pub fn number<I:U8Input>(i:I) -> ParseResult<I, i32> {
    or(i,
       parser!{
           token(b'-');
           let v = hex() <|> dec();
           ret (-(v as i32));
       },
       parser!{
           let v = hex() <|> dec();
           ret (v as i32);
       }
    )
}

pub fn is_horizontal_space(c: u8) -> bool { c == b' ' || c == b'\t' }
pub fn is_space(c: u8) -> bool { c == b' ' }

pub fn horizontal_space<I:U8Input>(i:I) -> ParseResult<I,()> {
    parse!{i;
        skip_while(is_horizontal_space);
        ret ();
    }
}

pub fn comma_sep<I:U8Input>(i:I) -> ParseResult<I, ()> {
    parse!{i;
        skip_while(is_horizontal_space);
        token(b',');
        skip_while(is_horizontal_space);

        ret ();
    }
}

pub fn word<I:U8Input>(i:I) -> ParseResult<I, String> {
    take_while1(i, is_alphanumeric).bind(|i,v| {
        match String::from_utf8(v.into_vec()) {
            Ok(v) => i.ret(v),
            Err(e) => i.err(From::from(e))
        }
    })
}

pub fn const_item<I:U8Input, R>(i:I, b:&[u8], r:R) -> ParseResult<I, R> {
    string(i,b).map(|_| r).bind(|i,v| i.ret(v))
}

pub fn wrap_item<I:U8Input, R, F:FnOnce(String) -> R>(i:I, f:F)
    -> ParseResult<I, R>
{
    word(i).map(f).bind(|i,v| i.ret(v))
}

