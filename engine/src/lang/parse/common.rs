use std::collections::HashSet;

use crate::lang::syntax::Span;

//mod directive;
//pub mod lexer;

// The third [T] parameter is so we can have [Carrier]s with the same [I]nput
// types without running afoul of the canonicity check
#[allow(dead_code)]
#[derive(Debug)]
pub enum Carrier<I, E, T> {
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
    // This is [Infallible] to ensure that it's never constructed
    Tag(T, std::convert::Infallible),
}

pub trait GenericParseErrorHandler<I: std::hash::Hash + Eq>: Sized {
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
    ) -> Self {
        let _ = unclosed;
        Self::expected(span, vec![Some(expected)].into_iter().collect(), found)
    }
}

impl<I, E, T> Carrier<I, E, T>
where
    I: PartialEq + Eq + std::hash::Hash + Clone,
    E: GenericParseErrorHandler<I>,
{
    pub fn generic_parse_error<Iter>(
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

    pub fn unclosed_delimiter_impl(
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

    pub fn merge_impl(mut self, other: Self) -> Self {
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
                    expected.insert(c.clone());
                }
            }
            (
                GenericParseError { expected, .. },
                GenericUnclosedDelimiter { expected: c, .. },
            ) => {
                expected.insert(Some(c.clone()));
            }
            (Tag(_, seal), _) => match *seal {},
            (_, Tag(_, seal)) => match *seal {},
        }

        self
    }

    pub fn into(self) -> E {
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
            Self::Tag(_, seal) => match seal {},
        }
    }
}
