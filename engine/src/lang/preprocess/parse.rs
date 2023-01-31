use std::rc::Rc;

use chumsky::{error::Error as ChumskyError, prelude::*};
use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::{
    lang::{
        parse,
        parse::common::GenericParseErrorHandler,
        syntax::{
            span::{stream_spanned, Source},
            Directive, GroupKind, MacroBody, Node, Span, Spanned, Token,
            TokenGroup, Tree,
        },
    },
    plumbing::*,
    types::{string_with_vars, StringWithVars},
};

#[cfg(test)]
mod tests;

// This needs to be ['static] to make some of the recursive parsers work, and
// is probably good practice anyways.
pub trait ErrorHandler:
    GenericParseErrorHandler<char> + string_with_vars::ParseErrorHandler + 'static
{
    fn unclosed_comment(span: Span) -> Self;

    fn unclosed_quotes(span: Span) -> Self;

    fn bad_directive(span: Span) -> Self;

    fn expected_end_of_line(why: &'static str, span: Span) -> Self;

    fn unclosed_if(which: &'static str, span: Span) -> Self;

    fn define_duplicate_arg(span: Span) -> Self;

    fn define_unmatched_start_quote(span: Span) -> Self;

    fn define_unmatched_end_quote(span: Span) -> Self;

    fn unmatched_else(span: Span) -> Self;

    fn unmatched_endif(span: Span) -> Self;

    fn unmatched_paren(span: Span) -> Self;
}

#[derive(Copy, Clone)]
pub enum PpSyntaxKind {
    Directive,
    DirectiveEnd(&'static str),
    IfBody(&'static str),
}

enum W {}

type Carrier<E> = parse::common::Carrier<char, E, W>;

impl<E> ChumskyError<char> for Carrier<E>
where
    E: ErrorHandler,
{
    type Span = Span;
    type Label = PpSyntaxKind;

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
                unclosed_span,
                unclosed: _,
                span: _,
                expected: _,
                found: _,
            } => unclosed_span,
            Carrier::Specific(_) => return self,
            Carrier::Tag(_, seal) => match seal {},
        };

        use PpSyntaxKind::*;
        Carrier::Specific(match label {
            Directive => E::bad_directive(span),
            DirectiveEnd(why) => E::expected_end_of_line(why, span),
            IfBody(which) => E::unclosed_if(which, span),
        })
    }
}

fn line_comment<E>() -> impl Parser<char, (), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
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
fn block_comment<E>() -> impl Parser<char, (), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let line_comment = line_comment();

    recursive(|block_comment| {
        just("/*")
            .ignored()
            .map_with_span(|_, span| span)
            .then(
                block_comment
                    .or(choice((
                        line_comment.clone(),
                        none_of("*/").ignored(),
                        just("*").ignored().then_ignore(none_of("/").rewind()),
                        just("/").ignored().then_ignore(none_of("*").rewind()),
                    )))
                    .repeated()
                    .ignored(),
            )
            .then(just("*/").ignored().or_not())
            // We need to do it this way instead of with [delimited_by] because
            // [delimited_by] doesn't properly mark the location of the
            // starting delimiter.
            .try_map(|((opening_span, _), ending), _| match ending {
                Some(_) => Ok(()),
                None => {
                    Err(Carrier::Specific(E::unclosed_comment(opening_span)))
                }
            })
    })
    .ignored()
}

fn rest_of_line<E>() -> impl Parser<char, String, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
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

fn non_nl_whitespace<E>() -> impl Parser<char, char, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    filter(move |c: &char| c.is_whitespace() && *c != '\n')
}

// TODO: We should just expose the [Parser] impl for [StringWithVars] directly
// instead of going through this song and dance. It's a bit annoying to do so
// because the error impls won't line up very well.
fn parse_string_with_vars<E, F>(
    s: String,
    span: &Span,
    fail: &mut F,
) -> StringWithVars
where
    E: ErrorHandler,
    F: FnMut(Carrier<E>) -> () + ?Sized,
{
    match StringWithVars::parse(Some(span.source()), Some(span.start()), &s) {
        Ok(s) => s,
        Err(errs) => {
            for err in errs {
                fail(Carrier::Specific(err))
            }
            StringWithVars::from(vec![string_with_vars::text(s)])
        }
    }
}

fn command_line<E>(
) -> impl Parser<char, (RelativePathBuf, Vec<StringWithVars>), Error = Carrier<E>>
       + Clone
where
    E: ErrorHandler,
{
    let prog = path().padded_by(non_nl_whitespace());

    let unquoted_arg = none_of("\" ").repeated();

    let quoted_arg = just('"')
        .ignore_then(just("\\\"").to('"').or(none_of("\"")).repeated())
        .then_ignore(just('"'));

    let arg = unquoted_arg
        .or(quoted_arg)
        .padded_by(non_nl_whitespace())
        .collect::<String>();

    prog.then(arg.map_with_span(|s, span| (s, span)).repeated())
        .validate(|(prog, args), _span, fail| {
            let args = args
                .into_iter()
                .map(|(arg, span)| parse_string_with_vars(arg, &span, fail))
                .collect();
            (prog, args)
        })
}

fn path<E>() -> impl Parser<char, RelativePathBuf, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    just('"')
        .ignored()
        .or_not()
        .then_with(|quote| {
            let accept_space = quote.is_some();

            let possible_path_character = filter(move |c: &char| {
                c.is_alphanumeric() || *c == '_' || (*c == ' ' && accept_space)
            });

            possible_path_character
                .repeated()
                .at_least(1)
                .collect::<String>()
                .separated_by(just('/').or(just('\\')))
                .map(move |segments| {
                    let mut path = RelativePathBuf::new();

                    for segment in segments {
                        path.push(segment);
                    }

                    (quote, path)
                })
        })
        .then(just('"').ignored().or_not())
        .validate(|((startquote, result), endquote), span, fail| {
            match (startquote, endquote) {
                (None, None) | (Some(()), Some(())) => (),
                _ => fail(Carrier::Specific(E::unclosed_quotes(span))),
            };
            result
        })
}

fn if_<E>(
    tree: impl Parser<char, Tree, Error = Carrier<E>> + Clone,
) -> impl Parser<char, (Tree, Tree), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    tree.clone()
        .then(
            just("#else")
                .ignore_then(tree)
                .then_ignore(just("#endif"))
                .map(Some)
                .or(just("#endif").to(None)),
        )
        .map(|(then, else_)| (then, else_.unwrap_or(Tree(vec![]))))
}

// XXX: Quotes are the actual bane of my existence.
fn define<E>() -> impl Parser<char, Directive, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let header = text::ident()
        .then_ignore(token_separator())
        .then(
            just('(')
                .map_with_span(flat_snd)
                .then(
                    text::ident()
                        .padded_by(token_separator())
                        .map_with_span(id2)
                        .separated_by(
                            just(',').then_ignore(token_separator().or_not()),
                        )
                        .then(just(')').ignored().or_not()),
                )
                .validate(|(span, (args, close_paren)), _, fail| {
                    match close_paren {
                        Some(_) => args,
                        None => {
                            fail(Carrier::Specific(E::unmatched_paren(span)));
                            args
                        }
                    }
                })
                .or_not(),
        )
        .validate(|(name, args), _, fail| {
            let args = match args {
                None => return (name, None),
                Some(args) => args,
            };

            let mut argset = IndexSet::new();

            for (arg, span) in args {
                let not_already_present = argset.insert(arg);

                if !not_already_present {
                    fail(Carrier::Specific(E::define_duplicate_arg(span)));
                }
            }

            (name, Some(argset))
        });

    let body = just('\"')
        .map_with_span(flat_snd)
        .or_not()
        .then(
            token_group()
                .map_with_span(id2)
                .separated_by(token_separator()),
        )
        .then(just('\"').map_with_span(flat_snd).or_not())
        .validate(|((startquote, body), endquote), _, fail| {
            match (startquote, endquote) {
                (Some(_), Some(_)) | (None, None) => (),
                (Some(span), None) => fail(Carrier::Specific(
                    E::define_unmatched_start_quote(span),
                )),
                (None, Some(span)) => {
                    fail(Carrier::Specific(E::define_unmatched_end_quote(span)))
                }
            }

            body
        })
        .or_not();

    header
        .then_ignore(token_separator())
        .then(body)
        .map(|((name, args), body)| match body {
            None => Directive::Define(name, args, MacroBody::Empty),
            Some(body) => Directive::Define(name, args, MacroBody::Macro(body)),
        })
        .then_ignore(
            end_of_line().labelled(PpSyntaxKind::DirectiveEnd("define")),
        )
}

fn directive<E>(
    tree: impl Parser<char, Tree, Error = Carrier<E>> + Clone,
) -> impl Parser<char, Directive, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    just('#').ignore_then(
        choice((
            just("define")
                .ignore_then(token_separator())
                .ignore_then(define()),
            just("include")
                .ignore_then(token_separator())
                .ignore_then(path())
                .then_ignore(
                    end_of_line()
                        .labelled(PpSyntaxKind::DirectiveEnd("include")),
                )
                .map(Directive::Include),
            just("incbin")
                .ignore_then(token_separator())
                .ignore_then(path())
                .then_ignore(
                    end_of_line()
                        .labelled(PpSyntaxKind::DirectiveEnd("incbin")),
                )
                .map(Directive::Incbin),
            just("incext")
                .ignore_then(token_separator())
                .ignore_then(command_line())
                .then_ignore(
                    end_of_line()
                        .labelled(PpSyntaxKind::DirectiveEnd("incext")),
                )
                .map(|(prog, args)| Directive::Incext(prog, args)),
            just("inctevent")
                .ignore_then(token_separator())
                .ignore_then(command_line())
                .then_ignore(
                    end_of_line()
                        .labelled(PpSyntaxKind::DirectiveEnd("inctevent")),
                )
                .map(|(prog, args)| Directive::Inctevent(prog, args)),
            just("ifdef")
                .ignore_then(token_separator())
                .ignore_then(text::ident())
                .then_ignore(
                    end_of_line().labelled(PpSyntaxKind::DirectiveEnd("ifdef")),
                )
                .then(if_(tree.clone()).labelled(PpSyntaxKind::IfBody("ifdef")))
                .map(|(arg, (then, else_))| Directive::IfDef(arg, then, else_)),
            just("ifndef")
                .ignore_then(token_separator())
                .ignore_then(text::ident())
                .then_ignore(
                    end_of_line()
                        .labelled(PpSyntaxKind::DirectiveEnd("ifndef")),
                )
                .then(
                    if_(tree.clone()).labelled(PpSyntaxKind::IfBody("ifndef")),
                )
                .map(|(arg, (then, else_))| {
                    Directive::IfNDef(arg, then, else_)
                }),
            just("pool")
                .then_ignore(non_nl_whitespace().repeated())
                .then_ignore(end_of_line())
                .to(Directive::Pool),
            just("undef")
                .ignore_then(token_separator())
                .ignore_then(text::ident())
                .then_ignore(non_nl_whitespace().repeated())
                .then_ignore(end_of_line())
                .map(Directive::Undef),
            just("endif").try_map(|_, span| {
                Err(Carrier::Specific(E::unmatched_endif(span)))
            }),
            just("else").try_map(|_, span| {
                Err(Carrier::Specific(E::unmatched_else(span)))
            }),
        ))
        .labelled(PpSyntaxKind::Directive),
    )
}

fn number<E>() -> impl Parser<char, (String, usize), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
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

    hex.or(bin_or_dec)
}

fn quoted_string<E>() -> impl Parser<char, String, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let escape = just('\\').ignore_then(choice((
        just('\\'),
        just('/'),
        just('"'),
        just('\n').to(' '),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )));

    just('"')
        .ignored()
        .map_with_span(|_, span| span)
        .then(
            filter(|c| *c != '\\' && *c != '"' && *c != '\n')
                .or(escape)
                .repeated(),
        )
        .then(just('"').ignored().or_not())
        .try_map(|((opening_span, contents), ending), _| match ending {
            Some(_) => Ok(contents),
            None => Err(Carrier::Specific(E::unclosed_quotes(opening_span))),
        })
        .collect::<String>()
}

fn token_separator<E>() -> impl Parser<char, (), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    block_comment()
        .or(just('\\').ignore_then(just('\n')).ignored())
        .or(non_nl_whitespace().ignored())
        .repeated()
        .ignored()
}

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
fn end_of_line<E>() -> impl Parser<char, (), Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    filter(move |c: &char| c.is_whitespace() && *c != '\n')
        .ignored()
        .or(block_comment())
        .repeated()
        .or_not()
        .then(choice((just('\n').ignored(), line_comment())))
        .to(())
}

fn token<E>() -> impl Parser<char, Token, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let ident = text::ident();
    let number = number();
    let quoted_string = quoted_string();

    choice((
        ident.map(|s| Token::Ident(Rc::new(s))),
        number.map(|(payload, radix)| Token::Number { payload, radix }),
        quoted_string.map(Token::QuotedString),
        just(':').to(Token::Colon),
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
            just(';').to(Token::Semi),
            just('|').to(Token::Bar),
            just('^').to(Token::Caret),
            just('.').to(Token::Dot),
            just("<<").to(Token::LShift),
            just(">>").to(Token::RShift),
            just(",").to(Token::Comma),
            just("<").to(Token::LAngle),
            just(">").to(Token::RAngle),
        )),
    ))
}

// TODO: We should have better error reporting/recovery for mismatched
// parentheses. Try using the [nested_delimiters] recovery strategy.
fn delimited_group_parser<E>(
    token_group: impl Parser<char, TokenGroup, Error = Carrier<E>> + Clone,
    l: &'static str,
    r: &'static str,
    kind: GroupKind,
) -> impl Parser<char, TokenGroup, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    token_group
        .clone()
        .map_with_span(id2)
        .separated_by(token_separator())
        .delimited_by(just(l), just(r))
        .map(move |members| TokenGroup::Group { kind, members })
}

fn token_group<E>() -> impl Parser<char, TokenGroup, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    recursive(|toks| {
        choice((
            token().map_with_span(|t, span| TokenGroup::Single((t, span))),
            delimited_group_parser(toks.clone(), "(", ")", GroupKind::Paren),
            delimited_group_parser(toks.clone(), "[", "]", GroupKind::Square),
            delimited_group_parser(toks.clone(), "{", "}", GroupKind::Curly),
        ))
        .padded_by(token_separator().or_not())
    })
}

fn line<E>(
) -> impl Parser<char, Vec<Spanned<TokenGroup>>, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    token_group()
        .map_with_span(id2)
        .separated_by(token_separator())
        .then_ignore(end_of_line())
}

fn node<E>(
    tree: impl Parser<char, Tree, Error = Carrier<E>> + Clone,
) -> impl Parser<char, Node, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    let message = just("MESSAGE")
        .padded_by(non_nl_whitespace().repeated())
        .ignore_then(rest_of_line().validate(|v, span, fail| {
            match StringWithVars::parse(
                Some(span.source()),
                Some(span.start()),
                &v,
            ) {
                Ok(s) => s,
                Err(errs) => {
                    for err in errs {
                        fail(Carrier::Specific(err))
                    }
                    StringWithVars::from(vec![string_with_vars::text(v)])
                }
            }
        }))
        .map(Node::Message);

    let directive = directive(tree).map(Node::Directive);

    let line = line().map(Node::Line);

    token_separator().ignore_then(choice((message, directive, line)))
}

pub fn tree<E>() -> impl Parser<char, Tree, Error = Carrier<E>> + Clone
where
    E: ErrorHandler,
{
    recursive(|tree| {
        node(tree)
            .map_with_span(|node, span| (node, span))
            .repeated()
            .map(Tree)
    })
}

pub fn parse<E>(source: &Source, s: impl AsRef<str>) -> Result<Tree, Vec<E>>
where
    E: ErrorHandler,
{
    // This is a huge hack. [line] explicitly looks for a newline, which
    // doesn't interact well with the end of the input. To fix this, we append
    // a newline to the input, because chumsky doesn't handle repetition of
    // "end of input" very well.
    //
    // One way around this might be to use [separated_by] somehow, but using
    // this combinator is difficult if there are block comments in inconvenient
    // places.
    let mut s: String = s.as_ref().to_owned();
    s.push('\n');
    let s = s;

    tree()
        .then_ignore(end())
        .parse(stream_spanned(Some(source), None, s))
        .map_err(|errs| errs.into_iter().map(Carrier::into).collect())
}
