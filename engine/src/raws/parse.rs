use chumsky::{error::Error as ChumskyError, prelude::*, text::ident};

use crate::lang::{
    parse::common::{Carrier as C, GenericParseErrorHandler},
    syntax::Span,
};

use super::*;

enum W {}

type Carrier<E> = C<char, E, W>;

pub trait ErrorHandler: GenericParseErrorHandler<char> + 'static {
    fn bad_number(span: Span) -> Self;

    fn id_too_big(s: String, span: Span) -> Self;
}

impl<E> ChumskyError<char> for Carrier<E>
where
    E: ErrorHandler,
{
    type Span = Span;
    type Label = std::convert::Infallible;

    fn expected_input_found<I>(
        span: Self::Span,
        expected: I,
        found: Option<char>,
    ) -> Self
    where
        I: IntoIterator<Item = Option<char>>,
    {
        Self::generic_parse_error(span, expected, found)
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: char,
        span: Self::Span,
        expected: char,
        found: Option<char>,
    ) -> Self {
        Self::unclosed_delimiter_impl(
            unclosed_span,
            unclosed,
            span,
            expected,
            found,
        )
    }

    fn merge(self, other: Self) -> Self {
        Self::merge_impl(self, other)
    }

    fn with_label(self, label: Self::Label) -> Self {
        match label {}
    }
}

enum Payload {
    // TODO: use nonempty vec for this variant
    Values(Vec<String>),
    Range { start: String, end: String },
}

struct Flag {
    name: String,
    payload: Option<Payload>,
}

fn flag<E>() -> impl Parser<char, Flag, Error = Carrier<E>>
where
    E: ErrorHandler,
{
    let flag_value = filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_')
        .repeated()
        .at_least(1)
        .collect::<String>();

    just('-').ignore_then(
        ident()
            .then(choice((
                just(':').ignore_then(flag_value.clone()).repeated().map(
                    |args| {
                        if args.len() == 0 {
                            None
                        } else {
                            Some(Payload::Values(args))
                        }
                    },
                ),
                just(':')
                    .ignore_then(flag_value.clone())
                    .then_ignore(just('-'))
                    .then(flag_value)
                    .map(|(start, end)| Some(Payload::Range { start, end })),
            )))
            .map(|(name, payload)| Flag { name, payload }),
    )
}

/*
   Code:
    Code syntax is the following:
     CodeName, ID, Length, flags
    If ID is 0, then it is ignored. If no Flags are used, last ','
    can be left out. Code is then followed by 0 or more parameters
    and ended with an empty line. If two codes have same names, they
    must have either different amount of parameters or one or more
    of the parameters must have different amount of dimensions.
*/

struct CodeHeader {
    name: String,
    id: u16,
    length: usize,
    flags: Vec<Flag>,
}

fn code<E>() -> impl Parser<char, CodeHeader, Error = Carrier<E>>
where
    E: ErrorHandler,
{
    ident()
        .then_ignore(just(','))
        .then((number().then_ignore(just(','))).padded())
        .then(number())
        .then((just(',').ignore_then(flag().repeated())).or_not())
        .try_map(|(((name, id), length), flags), span| {
            let (id, radix) = id;

            let id = parse_number(&id, radix, span.clone())?
                .try_into()
                .map_err({
                    let span_ = span.clone();
                    move |_| Carrier::Specific(E::id_too_big(id, span_))
                })?;

            let (length, radix) = length;

            let length = parse_number(&length, radix, span.clone())?;

            let flags = match flags {
                None => vec![],
                Some(flags) => flags,
            };

            Ok(CodeHeader {
                name,
                id,
                length,
                flags,
            })
        })
}

/*
   Parameter:
    Each code has 0 or more parameters. Syntax is following:
     ParameterName, Position, Length, flags
    If no Flags are used, last ',' can be left out. The position must
    be greater or equal than zero, or greater or equal than 2 if code
    has an ID. The Sum of Position and Length must be smaller than
    length of the code and the parameters can't use the same bits
    in code. There also must be white space before the ParameterName.
*/

struct Parameter {
    name: String,
    position: usize,
    length: usize,
    flags: Vec<Flag>,
}

// XXX: This shares the same structure as [code], just with mandatory
// leading whitespace. It'd be nice to share them.
fn parameter<E>() -> impl Parser<char, Parameter, Error = Carrier<E>>
where
    E: ErrorHandler,
{
    ident()
        .then_ignore(just(','))
        .then((number().then_ignore(just(','))).padded())
        .then(number())
        .then((just(',').ignore_then(flag().repeated())).or_not())
        .try_map(|(((name, position), length), flags), span| {
            let (position, radix) = position;

            let position = parse_number(&position, radix, span.clone())?;

            let (length, radix) = length;

            let length = parse_number(&length, radix, span.clone())?;

            let flags = match flags {
                None => vec![],
                Some(flags) => flags,
            };

            Ok(Parameter {
                name,
                position,
                length,
                flags,
            })
        })
}

// TODO: un-copypaste this from [preprocess::parse::number], and figure out how
// to make the error handler generic
fn number<E>() -> impl Parser<char, (String, usize), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let digits = filter(move |c: &char| c.is_digit(36))
        .repeated()
        .collect::<String>();

    let bin_or_dec = filter(move |c: &char| c.is_digit(10)).then(digits).map(
        |(leading, s): (char, String)| {
            let s = format!("{}{}", leading, s);

            if s.ends_with("b") || s.ends_with("B") {
                (format!("{}", &s[0..s.len() - 1]), 2)
            } else {
                (s, 10)
            }
        },
    );

    let hex = (just("0x").or(just("$")))
        .ignore_then(digits)
        .map(|s: String| (s, 16));

    hex.or(bin_or_dec)
}

fn parse_number<E>(
    s: &String,
    radix: usize,
    span: Span,
) -> Result<usize, Carrier<E>>
where
    E: ErrorHandler,
{
    usize::from_str_radix(s.as_str(), radix as u32)
        .map_err(move |_| Carrier::Specific(E::bad_number(span)))
}
