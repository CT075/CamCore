use std::collections::HashSet;

use super::syntax::{
    Directive, Location, MessageContent, Span, Token, WithLocation,
};

pub mod firstpass;

#[derive(Debug)]
pub enum Carrier<I, E> {
    GenericParseError {
        span: Span,
        expected: HashSet<Option<I>>,
        found: Option<I>,
    },
    GenericUnclosedDelimiter {
        unclosed_span: Span,
        unclosed: I,
        span: Span,
        expected: I,
        found: Option<I>,
    },
    Specific(E),
}

pub trait GenericParseErrorHandler<I> {
    fn expected(
        span: Span,
        expected: HashSet<Option<I>>,
        found: Option<I>,
    ) -> Self;

    fn unclosed_delimiter(
        span: Span,
        unclosed: I,
        expected: I,
        found: Option<I>,
    ) -> Self;
}

impl<I, E> Carrier<I, E>
where
    I: PartialEq + Eq + std::hash::Hash + Clone + Copy,
    E: GenericParseErrorHandler<I>,
{
    fn generic_parse_error<Iter>(
        span: Span,
        expected: Iter,
        found: Option<I>,
    ) -> Self
    where
        Iter: IntoIterator<Item = Option<I>>,
    {
        Self::GenericParseError {
            span,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    fn unclosed_delimiter_impl(
        unclosed_span: Span,
        unclosed: I,
        span: Span,
        expected: I,
        found: Option<I>,
    ) -> Self {
        Self::GenericUnclosedDelimiter {
            unclosed_span,
            unclosed,
            span,
            expected,
            found,
        }
    }

    fn merge_impl(mut self, other: Self) -> Self {
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

    fn into(self) -> E {
        match self {
            Self::Specific(e) => e,
            Self::GenericParseError {
                span,
                expected,
                found,
            } => E::expected(span, expected, found),
            Self::GenericUnclosedDelimiter {
                unclosed_span: _,
                unclosed,
                span,
                expected,
                found,
            } => E::unclosed_delimiter(span, unclosed, expected, found),
        }
    }
}
