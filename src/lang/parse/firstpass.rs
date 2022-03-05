// Lexing, directive parsing and MESSAGE

use crate::types::hkt::{ConstW, VecW, Witness};

use super::{Carrier, Directive, MessageContent, Span, Token, WithLocation};

use chumsky::{error::Error as ChumskyError, prelude::*};

#[cfg(test)]
mod tests;

// This needs to be ['static] to make some of the recursive parsers work, and
// is probably good practice anyways.
pub trait LexErrorHandler: 'static {
    fn unclosed_comment(span: Span) -> Self;

    fn bad_number(radix: u32, span: Span) -> Self;
}

#[derive(Copy, Clone)]
pub enum LexKind {
    BlockComment,
    Number(u32),
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
            Number(radix) => E::bad_number(radix, span),
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

// TODO: We should emit a different error if the comment contains at least
// one nested comment block. This is quite annoying to actually do due to how
// this parser is set up. If we have [/* /* */], then the *outermost* parser is
// the one that fails, meaning that we can't rely on [labelled] to tag the
// correct parser.
//
// Instead, we need to unroll the recursion one step, then somehow have the
// inner parser produce a result that tells whether any nested block comments
// were encountered, then use that result to produce a label. I wasn't able to
// get any initial attempts at this to work, and I think it's a niche enough
// problem that it's not worth spending more time on.
fn block_comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let line_comment = line_comment();

    recursive(|block_comment| {
        block_comment
            .or(choice((
                line_comment.clone(),
                none_of("*/").ignored(),
                just("*").ignored().then_ignore(none_of("/").rewind()),
                just("/").ignored().then_ignore(none_of("*").rewind()),
            )))
            .repeated()
            .ignored()
            .delimited_by(just("/*"), just("*/"))
    })
    .labelled(LexKind::BlockComment)
    .padded()
    .ignored()
}

fn comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let line_comment = line_comment();
    let block_comment = block_comment();

    line_comment.or(block_comment)
}

fn number<E>(
) -> impl Parser<char, (String, usize), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    // We can't use [text::int], because that function doesn't quite have the
    // error handling properties we want. Instead, we collect all identifier
    // characters and infer the radix, to be parsed later.

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

    hex.or(bin_or_dec).padded()
}

fn token<E>() -> impl Parser<char, Token, Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let ident = text::ident();
    let number = number();

    choice((
        ident.map(Token::Ident),
        number.map(|(payload, radix)| Token::Number { payload, radix }),
    ))
}

fn parser<'a, E>(
) -> impl Parser<char, Vec<OutImpl<'a, VecW>>, Error = Carrier<char, E>>
where
    E: LexErrorHandler,
{
    let line = todo();

    line.padded_by(comment().repeated()).padded().repeated()
}
