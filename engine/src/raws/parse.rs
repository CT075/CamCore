use chumsky::{error::Error as ChumskyError, prelude::*};

use crate::lang::{
    parse::common::{Carrier as C, GenericParseErrorHandler},
    syntax::Span,
};

use super::*;

enum W {}

type Carrier<E> = C<char, E, W>;

pub trait ErrorHandler: GenericParseErrorHandler<char> + 'static {
    //
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
