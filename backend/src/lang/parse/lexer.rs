// Lexing, basic directive disambiguation and MESSAGE

// TODO: The whole use of [Carrier], [LexKind], etc, could probably be
// simplified a lot with judicious use of [try_map]. This whole module was
// written before I had much experience with chumsky, so there's probably all
// sorts of low-hanging simplifications here.

// TODO: The entire parsing pipeline would be less complicated if [lex]
// produced a token tree (with matched parens) instead of a flat stream

use crate::{
    lang::syntax::{directive, directive::Unparsed, Span, SpannedW, Token},
    types::hkt::{IdentityW, VecW, Witness},
};

use super::{Carrier, Directive, GenericParseErrorHandler};

use chumsky::{error::Error as ChumskyError, prelude::*};

#[cfg(test)]
mod tests;

// This needs to be ['static] to make some of the recursive parsers work, and
// is probably good practice anyways.
pub trait LexErrorHandler: GenericParseErrorHandler<char> + 'static {
    fn unclosed_comment(span: Span) -> Self;

    fn unclosed_string_literal(span: Span) -> Self;

    fn bad_directive(span: Span) -> Self;
}

#[derive(Copy, Clone)]
pub enum LexKind {
    BlockComment,
    String,
    Directive,
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
        let span = match self {
            Carrier::GenericParseError {
                span,
                expected: _,
                found: _,
            } => span,
            Carrier::GenericUnclosedDelimiter {
                unclosed_span: _,
                unclosed: _,
                span,
                expected: _,
                found: _,
            } => span,
            Carrier::Specific(_) => return self,
        };

        use LexKind::*;
        Carrier::Specific(match label {
            BlockComment => E::unclosed_comment(span),
            String => E::unclosed_string_literal(span),
            Directive => E::bad_directive(span),
        })
    }
}

// This is a trick known as "higher-kinded data". The basic idea is that we
// want this type to be polymorphic over the "shape" of some data contained
// in a variant.
//
// In this particular case, there are two things we want to be polymorphic
// over.
//
// - We want to use the same datatype regardless of whether the inner syntax
//   is tagged with just a span (like after lexing a macro definition) or a
//   full location (for regular parsing).
// - Because EA is a line-based language, it's convenient to produce tokens one
//   line at a time as a sequence of [Vec<Token>]s. However, the next stage of
//   parsing is much more convenient as a flat stream. More concretely, it's
//   the difference between [Vec<Vec<Token>>] and [Vec<Token>].
//
// By using this seemingly more-complicated representation, we can ensure that
// all three variants are location-tagged in the same way.
//
// You can read more about this approach here:
//   https://reasonablypolymorphic.com/blog/higher-kinded-data/
//
// It's a bit uglier than what you'd see in Haskell, since Rust doesn't support
// higher kinded types as a first-class construct, so we have to get a bit
// cleverer with our embedding (see [hkt.rs]).
pub enum OutImpl<
    D: directive::Args,
    L: Witness<Token> + Witness<Directive<D>> + Witness<String>,
    F: Witness<<L as Witness<Token>>::This>,
> {
    Token(F::This),
    Directive(<L as Witness<Directive<D>>>::This),
    Message(<L as Witness<String>>::This),
}

impl<D, L, F> std::fmt::Debug for OutImpl<D, L, F>
where
    D: directive::Args,
    L: Witness<Token> + Witness<Directive<D>> + Witness<String>,
    F: Witness<<L as Witness<Token>>::This>,
    F::This: std::fmt::Debug,
    <L as Witness<Token>>::This: std::fmt::Debug,
    <L as Witness<Directive<D>>>::This: std::fmt::Debug,
    <L as Witness<String>>::This: std::fmt::Debug,
{
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::Token(ts) => fmt.debug_tuple("Token").field(ts).finish(),
            Self::Directive(ds) => {
                fmt.debug_tuple("Directive").field(ds).finish()
            }
            Self::Message(s) => fmt.debug_tuple("Message").field(s).finish(),
        }
    }
}

impl<D, L, F> PartialEq for OutImpl<D, L, F>
where
    D: directive::Args,
    L: Witness<Token> + Witness<Directive<D>> + Witness<String>,
    F: Witness<<L as Witness<Token>>::This>,
    F::This: Eq,
    <L as Witness<Token>>::This: Eq,
    <L as Witness<Directive<D>>>::This: Eq,
    <L as Witness<String>>::This: Eq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Token(t1), Self::Token(t2)) => t1 == t2,
            (Self::Directive(ds1), Self::Directive(ds2)) => ds1 == ds2,
            (Self::Message(m1), Self::Message(m2)) => m1 == m2,
            _ => false,
        }
    }
}

impl<D, L, F> Eq for OutImpl<D, L, F>
where
    D: directive::Args,
    L: Witness<Token> + Witness<Directive<D>> + Witness<String>,
    F: Witness<<L as Witness<Token>>::This>,
    F::This: Eq,
    <L as Witness<Token>>::This: Eq,
    <L as Witness<Directive<D>>>::This: Eq,
    <L as Witness<String>>::This: Eq,
{
}

pub type Out = OutImpl<Unparsed, SpannedW, IdentityW>;

fn non_nl_whitespace<E>(
) -> impl Parser<char, char, Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    filter(move |c: &char| c.is_whitespace() && *c != '\n')
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

// TODO: We should emit a different error if the comment contains at least
// one nested comment block. This is quite annoying to actually do due to how
// this parser is set up. If we have [/* /* */], then the *outermost* parser is
// the one that fails, meaning that we can't rely on [labelled] to tag the
// correct parser.
//
// My first thought was to unroll the recursion one step, then somehow have the
// inner parser produce a result that tells whether any nested block comments
// were encountered, then use that result to produce a label. I wasn't able to
// get any initial attempts at this to work, and I think it's a niche enough
// problem that it's not worth spending more time on.
//
// After gaining more experience with the library, I think there's probably a
// way to do this using the [nested_delimiters] recovery strategy.
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
    .ignored()
}

fn comment<E>() -> impl Parser<char, (), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let line_comment = line_comment();
    let block_comment = block_comment().padded();

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

fn quoted_string<E>(
) -> impl Parser<char, String, Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('\n').to(' '))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    just('"')
        .ignore_then(
            filter(|c| *c != '\\' && *c != '"' && *c != '\n')
                .or(escape)
                .repeated(),
        )
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled(LexKind::String)
}

pub fn token<E>() -> impl Parser<char, Token, Error = Carrier<char, E>>
where
    E: LexErrorHandler,
{
    let ident = text::ident();
    let number = number();
    let quoted_string = quoted_string();

    choice((
        ident.map(Token::Ident),
        number.map(|(payload, radix)| Token::Number { payload, radix }),
        quoted_string.map(Token::QuotedString),
        just(':').to(Token::Colon),
        just("--").to(Token::Emdash),
        just('-').to(Token::Dash),
        just('/')
            .then_ignore(none_of("/").rewind())
            .to(Token::Slash),
        just('*').to(Token::Star),
        just('+').to(Token::Plus),
        just('%').to(Token::Percent),
        just('&').to(Token::Ampersand),
        // we need this nested [choice] here because the parser bounds on
        // [choice] haven't been defined for tuples this big
        choice((
            just('|').to(Token::Bar),
            just('^').to(Token::Caret),
            just('.').to(Token::Dot),
            just("<<").to(Token::LShift),
            just(">>").to(Token::RShift),
            just(",").to(Token::Comma),
            just("{").to(Token::LCurly),
            just("}").to(Token::RCurly),
            just("(").to(Token::LParen),
            just(")").to(Token::RParen),
            just("[").to(Token::LBrack),
            just("]").to(Token::RBrack),
            just("<").to(Token::LAngle),
            just(">").to(Token::RAngle),
        )),
    ))
}

// This is roughly the "end of the line", but also includes [;].
//
// TODO: This is too complicated. We should just parse [;] in [token], and just
// make the overall lexer something like [line().padded_by(just('\n'))].
//
// TODO: Currently, in the following input, [A] and [B] are considered to be on
// "the same line".
//
// ```
// A /* comment with a line break
//    */ B
// ```
//
// Arguments could be made either way for whether this is intuitive, but it
// isn't sufficient to just use [block_comment] as the end of the statement,
// because we also have to handle this case:
//
// ```
// A /* comment without a line break */ B
// ```
fn statement_break<E>(
) -> impl Parser<char, Token, Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    choice((just('\n').ignored(), just(';').ignored(), line_comment()))
        .padded_by(
            filter(move |c: &char| c.is_whitespace() && *c != '\n')
                .repeated()
                .ignored(),
        )
        .to(Token::Break)
}

fn rest_of_line<E>(
) -> impl Parser<char, String, Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    choice((
        block_comment().to(None),
        none_of("\\/\n").map(Some),
        just('/').then_ignore(none_of("/*").rewind()).map(Some),
        just('\\').then_ignore(just('\n')).map(|_| Some(' ')),
        just('\\').map(Some),
    ))
    .repeated()
    .map(|cs| {
        cs.into_iter()
            .filter_map(|opt| opt)
            .collect::<String>()
            .trim()
            .to_string()
    })
    .then_ignore(just('\n').ignored().or(line_comment()))
}

// For the sake of future error reporting, when we mark the span of this line,
// we mark the actual text of the directive, not counting the start.
//
// TODO: This is bugged. Consider:
//
//   #incext "program.exe // the slashes are an argument"
//
// This will get parsed as
//
//   Incext("\"program.exe ")
//
// without correctly catching the slashes. There's a similar issue with block
// comments in the string.
//
// That said, if you code like this, I hate you anyway and you deserve the
// errors.
fn directive<E>(
) -> impl Parser<char, (Directive<Unparsed>, Span), Error = Carrier<char, E>> + Clone
where
    E: LexErrorHandler,
{
    just('#').ignore_then(
        choice((
            just("define")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Define(line), span)),
            just("include")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Include(line), span)),
            just("incbin")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Incbin(line), span)),
            just("incext")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Incext(line), span)),
            just("inctevent")
                .or(just("inctext"))
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Inctevent(line), span)),
            just("ifdef")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::IfDef(line), span)),
            just("ifndef")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::IfNDef(line), span)),
            just("else")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Else(line), span)),
            just("endif")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Endif(line), span)),
            just("pool")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Pool(line), span)),
            just("undef")
                .then_ignore(non_nl_whitespace().repeated().at_least(1))
                .ignore_then(rest_of_line())
                .map_with_span(|line, span| (Directive::Undef(line), span)),
        ))
        .labelled(LexKind::Directive),
    )
}

fn parser<E>() -> impl Parser<
    char,
    Vec<OutImpl<Unparsed, SpannedW, VecW>>,
    Error = Carrier<char, E>,
>
where
    E: LexErrorHandler,
{
    let line = token()
        .map_with_span(|v, span| (v, span))
        .padded_by(
            block_comment()
                .or(just('\\').ignore_then(just('\n')).ignored())
                .or(filter(move |c: &char| c.is_whitespace() && *c != '\n')
                    .ignored())
                .repeated(),
        )
        .repeated()
        .then(statement_break().map_with_span(|v, span| (v, span)))
        .map(|(mut tokens, brk)| {
            tokens.push(brk);
            OutImpl::Token(tokens)
        });

    let message = just("MESSAGE")
        .padded_by(non_nl_whitespace().repeated())
        .ignore_then(rest_of_line().map_with_span(|v, span| (v, span)))
        .map(OutImpl::Message);

    let directive = directive().map(OutImpl::Directive).padded();

    message
        .or(directive)
        .or(line)
        .padded_by(comment().repeated())
        .padded()
        .repeated()
}

fn flatten(v: Vec<OutImpl<Unparsed, SpannedW, VecW>>) -> Vec<Out> {
    v.into_iter()
        .flat_map(|item| match item {
            OutImpl::Token(toks) => toks.into_iter().map(Out::Token).collect(),
            OutImpl::Directive(d) => {
                vec![Out::Directive(d)]
            }
            OutImpl::Message(msg) => vec![Out::Message(msg)],
        })
        .collect()
}

pub fn lex<E>(
    s: impl AsRef<str>,
    span: Option<&Span>,
) -> Result<Vec<Out>, Vec<E>>
where
    E: LexErrorHandler,
{
    let Span { start, end: _ } = span.unwrap_or(&Span { start: 0, end: 0 });

    // This is a huge hack. Currently, chumsky does not handle repetition of
    // things like "end of line or end of input" very well, so we force an
    // end-of-line at the end of the input.
    let mut s: String = s.as_ref().to_owned();
    s.push('\n');
    let s = s;

    parser()
        .then_ignore(end())
        .parse(chumsky::stream::Stream::from_iter(
            *start..*start + s.len(),
            s.chars()
                .enumerate()
                .map(|(i, c)| (c, i + start..i + start + 1)),
        ))
        .map(flatten)
        .map_err(|errs| errs.into_iter().map(Carrier::into).collect())
}
