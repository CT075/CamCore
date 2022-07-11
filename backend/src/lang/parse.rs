// XXX: The overall parsing infrastructure uses a bunch of different techniques
// to do the same thing. For example, there are a few places where we use
// [chumsky::error::Simple] instead of [Carrier] for inner parser errors. A
// good LHF fix would be to clean these up.

pub mod common {
    use std::collections::HashSet;

    use crate::lang::syntax::Span;

    //mod directive;
    //pub mod lexer;

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
            Self::expected(
                span,
                vec![Some(expected)].into_iter().collect(),
                found,
            )
        }
    }

    impl<I, E> Carrier<I, E>
    where
        I: PartialEq + Eq + std::hash::Hash + Clone + Copy,
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
            }
        }
    }
}
