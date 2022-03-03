// Lexing, directive parsing and MESSAGE

use crate::types::hkt::{Apply, ConstW, VecW, Witness};

use super::{Carrier, Directive, MessageContent, Span, Token, WithLocation};

use chumsky::{error::Error as ChumskyError, prelude::*};

#[cfg(test)]
mod tests;

// This needs to be ['static] to make some of the recursive parsers work, and
// is probably good practice anyways.
pub trait LexErrorHandler: 'static {
    fn unclosed_comment(span: Span) -> Self;
}

#[derive(Copy, Clone)]
pub enum LexKind {
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

// This is a trick known as "higher-kinded data", where we know that the
// [Token] variant will always contain something that uses the [Token] type,
// but want to be generic over the "shape" of that something. In this case,
// it's more convenient to parse by lines, in which we'll want to use
// [Vec<Token>]. However, we'd like the actual output of this phase to just be
// a flat list of tokens, so that variant should just contain regular [Token].
//
// You can read more about this approach here:
//   https://reasonablypolymorphic.com/blog/higher-kinded-data/
//
// It's a bit uglier than what you'd see in Haskell, since Rust doesn't support
// higher kinded types as a first-class construct, so we have to get a bit
// cleverer with our embedding (see [hkt.rs]).
enum OutImpl<'a, F: Witness<WithLocation<'a, Token>>> {
    Token(F::This),
    Directive(WithLocation<'a, Directive>),
    Message(Vec<WithLocation<'a, MessageContent>>),
}

pub type Out<'a> = OutImpl<'a, ConstW>;

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

fn comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let line_comment = line_comment();
    let block_comment = block_comment();

    line_comment.or(block_comment)
}

fn parser<'a, E>() -> impl Parser<char, Vec<OutImpl<'a, VecW>>, Error = Carrier<char, E>>
where
    E: LexErrorHandler,
{
    let line = todo();

    line.padded_by(comment().repeated()).padded().repeated()
}
