use std::collections::HashSet;

use super::syntax::{Directive, MessageContent, Span, Token, WithLocation};

use chumsky::{error::Error as ChumskyError, prelude::*};

#[cfg(test)]
mod tests;

#[derive(Debug)]
enum Carrier<I, E> {
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
    fn expected(span: Span, expected: HashSet<Option<I>>, found: Option<I>) -> Self;

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
{
    fn generic_parse_error<Iter>(span: Span, expected: Iter, found: Option<I>) -> Self
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
}

// Lexing, directive parsing and MESSAGE

// This needs to be ['static] to make some of the recursive parsers work, and
// is probably good practice anyways.
pub trait LexErrorHandler: 'static {
    fn unclosed_comment(span: Span) -> Self;
}

#[derive(Copy, Clone)]
enum LexKind {
    BlockComment,
}

impl<E> ChumskyError<char> for Carrier<char, E>
where
    E: LexErrorHandler,
{
    type Span = Span;
    type Label = LexKind;

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
        Self::unclosed_delimiter_impl(unclosed_span, unclosed, span, expected, found)
    }

    fn merge(self, other: Self) -> Self {
        Self::merge_impl(self, other)
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

        use LexKind::*;
        Carrier::Specific(match label {
            BlockComment => E::unclosed_comment(span),
        })
    }
}

enum FirstPassOut {
    Token(Token),
    Directive(Directive),
    Message(Vec<MessageContent>),
}

fn line_comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    just("//")
        .then(take_until(
            none_of("\\").then_ignore(just('\n')).ignored().or(end()),
        ))
        .padded()
        .ignored()
}

fn block_comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let line_comment = line_comment();

    recursive(|block_comment| {
        just("/*").ignored().then_ignore(recursive(|in_comment| {
            choice((
                line_comment.then_ignore(in_comment.clone()),
                just("*/").ignored(),
                block_comment.then_ignore(in_comment.clone()),
                none_of("/*")
                    .repeated()
                    .at_least(1)
                    .ignored()
                    .then_ignore(in_comment.clone()),
                one_of("*/").ignored().then_ignore(in_comment.clone()),
            ))
        }))
    })
    .padded()
    .ignored()
    .labelled(LexKind::BlockComment)
}

fn first_pass<'a, E>(
) -> impl Parser<char, Vec<WithLocation<'a, FirstPassOut>>, Error = Carrier<char, E>>
where
    E: LexErrorHandler,
{
    let token = todo();

    let line_comment = line_comment();

    let block_comment = block_comment();

    let comment = line_comment.or(block_comment);

    token.padded_by(comment.repeated()).padded().repeated()
}
