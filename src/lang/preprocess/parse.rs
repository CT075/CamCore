use std::collections::HashSet;

use core::ops::Range;

use chumsky::prelude::*;

use super::{syntax::Statement, PreprocessParseErrorHandler};

enum Carrier<E> {
    GenericParseError {
        span: Range<usize>,
        expected: HashSet<Option<char>>,
        found: Option<char>,
    },
    GenericUnclosedDelimiter {
        unclosed_span: Range<usize>,
        unclosed: char,
        span: Range<usize>,
        expected: char,
        found: Option<char>,
    },
    Specific(E),
}

enum PreprocessParseErrorKind {
    GenericExpected {
        what: &'static str,
        why: &'static str,
    },
    TooManyArgs {
        directive: &'static str,
        expected_amount: usize,
    },
    BadMacroArg,
    EmptyBody,
}

impl<E> chumsky::error::Error<char> for Carrier<E>
where
    E: PreprocessParseErrorHandler,
{
    type Span = Range<usize>;
    type Label = PreprocessParseErrorKind;

    fn expected_input_found<I>(
        span: Self::Span,
        expected: I,
        found: Option<char>,
    ) -> Self
    where
        I: IntoIterator<Item = Option<char>>,
    {
        Carrier::GenericParseError {
            span,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: char,
        span: Self::Span,
        expected: char,
        found: Option<char>,
    ) -> Self {
        Carrier::GenericUnclosedDelimiter {
            unclosed_span,
            unclosed,
            span,
            expected,
            found,
        }
    }

    fn merge(mut self, other: Self) -> Self {
        use Carrier::*;

        match (&mut self, &other) {
            (Specific(_) | GenericUnclosedDelimiter { .. }, _) => (),
            (_, Specific(_)) => return other,
            (
                GenericParseError { expected, .. },
                GenericParseError {
                    expected: expected_,
                    ..
                },
            ) => {
                for c in expected_ {
                    expected.insert(*c);
                }
            }
            (
                GenericParseError { expected, .. },
                GenericUnclosedDelimiter { expected: c, .. },
            ) => {
                expected.insert(Some(*c));
            }
        }

        self
    }

    fn with_label(self, label: Self::Label) -> Self {
        let (span, found) = match self {
            Carrier::GenericParseError {
                span,
                expected: _,
                found,
            } => (span, found),
            Carrier::GenericUnclosedDelimiter {
                unclosed_span: _,
                unclosed: _,
                span,
                expected: _,
                found,
            } => (span, found),
            Carrier::Specific(_) => return self,
        };

        use PreprocessParseErrorKind::*;
        Carrier::Specific(match label {
            GenericExpected { what, why } => E::expected(what, found, why, span),
            TooManyArgs {
                directive,
                expected_amount,
            } => E::too_many_args(directive, expected_amount, span),
            BadMacroArg => E::bad_macro_arg(span),
            EmptyBody => E::empty_body(span),
        })
    }
}

fn parser<E>() -> impl Parser<char, Vec<Statement>, Error = Carrier<E>>
where
    E: PreprocessParseErrorHandler,
{
    todo()
}
