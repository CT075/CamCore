// directives

use std::collections::HashSet;

use indexmap::IndexSet;
use relative_path::RelativePathBuf;

use crate::{
    lang::syntax::{
        directive,
        directive::{Definition, NoPayload, Parsed, Unparsed},
        Span, Spanned, Token,
    },
    plumbing::*,
    types::{string_with_vars, StringWithVars},
};

use super::{Directive, GenericParseErrorHandler};

use chumsky::{
    error::{Error as ChumskyError, Simple},
    prelude::*,
    text,
};

enum HandledError<E> {
    Generic(Simple<char>),
    Known(E),
}

pub trait DirectiveParseErrorHandler: GenericParseErrorHandler<char> {
    fn unclosed_quotes(span: Span) -> Self;

    fn bad_path_char(c: char, span: Span) -> Self;

    fn only_one_arg_expected(why: Directive<NoPayload>, span: Span) -> Self;

    fn no_arg_expected(why: Directive<NoPayload>, span: Span) -> Self;

    fn error_in_template(
        what: &'static str,
        why: Directive<NoPayload>,
        span: Span,
    ) -> Self;

    fn macro_duplicate_arg(span: Span) -> Self;

    fn macro_contains_directive(span: Span) -> Self;

    fn macro_contains_message(span: Span) -> Self;

    fn macro_contains_hash(span: Span) -> Self;
}

#[derive(Clone, Debug)]
enum Label {
    OnePath(Directive<NoPayload>),
    Identifier(Directive<NoPayload>),
}

impl<E> HandledError<E>
where
    E: GenericParseErrorHandler<char>,
{
    fn into(self) -> E {
        match self {
            Self::Known(e) => e,
            Self::Generic(s) => E::expected(
                s.span(),
                s.expected().map(|&c| c).collect(),
                s.found().map(|&c| c),
            ),
        }
    }
}

impl<E> ChumskyError<char> for HandledError<E>
where
    E: DirectiveParseErrorHandler,
{
    type Span = Span;
    type Label = Label;

    fn expected_input_found<I>(
        span: Self::Span,
        expected: I,
        found: Option<char>,
    ) -> Self
    where
        I: IntoIterator<Item = Option<char>>,
    {
        Self::Generic(Simple::expected_input_found(span, expected, found))
    }

    fn unclosed_delimiter(
        unclosed_span: Self::Span,
        unclosed: char,
        span: Self::Span,
        expected: char,
        found: Option<char>,
    ) -> Self {
        Self::Generic(Simple::unclosed_delimiter(
            unclosed_span,
            unclosed,
            span,
            expected,
            found,
        ))
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Generic(_), Self::Known(e))
            | (Self::Known(e), Self::Generic(_)) => Self::Known(e),
            (Self::Generic(a), Self::Generic(b)) => {
                Self::Generic(Simple::merge(a, b))
            }
            (Self::Known(_), Self::Known(_)) =>
                panic!("BUG: Tried to merge two [Known] errors in directive preprocessor"),
        }
    }

    fn with_label(self, label: Self::Label) -> Self {
        match (&self, label) {
            (Self::Known(_), _) => self,
            (Self::Generic(s), Self::Label::OnePath(why)) => match s.found() {
                None => self,
                Some(c) => Self::Known(if c.is_whitespace() {
                    E::only_one_arg_expected(why, s.span())
                } else {
                    E::bad_path_char(*c, s.span())
                }),
            },
            (Self::Generic(_), _) => todo!(),
        }
    }
}

fn maybe_quoted<E, O>(
    inner: impl Parser<char, O, Error = HandledError<E>>,
) -> impl Parser<char, O, Error = HandledError<E>>
where
    E: DirectiveParseErrorHandler,
{
    let maybe_quote = just('"').ignored().or_not();

    maybe_quote.then(inner).then(maybe_quote).try_map(
        |((startquote, result), endquote), span| match (startquote, endquote) {
            (None, None) | (Some(()), Some(())) => Ok(result),
            _ => Err(HandledError::Known(E::unclosed_quotes(span))),
        },
    )
}

fn path<E>() -> impl Parser<char, RelativePathBuf, Error = HandledError<E>>
where
    E: DirectiveParseErrorHandler,
{
    let possible_path_character =
        filter(|c: &char| c.is_alphanumeric() || *c == '_' || *c == ' ');

    possible_path_character
        .repeated()
        .collect::<String>()
        .separated_by(just('/').or(just('\\')))
        .allow_trailing()
        .map(|segments| {
            let mut path = RelativePathBuf::new();

            for segment in segments {
                path.push(segment);
            }

            path
        })
}

fn single_identifier<E>(
    why: Directive<NoPayload>,
) -> impl Parser<char, String, Error = HandledError<E>>
where
    E: DirectiveParseErrorHandler,
{
    text::ident()
        .then_ignore(text::whitespace().repeated())
        .then_ignore(end())
        .labelled(Label::Identifier(why))
}

fn command_line<E>(
) -> impl Parser<char, (RelativePathBuf, Vec<String>), Error = HandledError<E>>
where
    E: DirectiveParseErrorHandler,
{
    let prog = maybe_quoted(path()).padded();

    let unquoted_arg = none_of("\" ").repeated();

    let quoted_arg = just('"')
        .ignore_then(none_of("\"").repeated())
        .then_ignore(just('"'));

    let arg = unquoted_arg.or(quoted_arg).padded().collect::<String>();

    prog.then(arg.repeated())
}

fn parse_and_map_errors<E, O>(
    parser: impl Parser<char, O, Error = HandledError<E>>,
    input: Spanned<String>,
) -> (Option<O>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let (s, span) = input;

    let s = chumsky::stream::Stream::from_iter(
        span.end..span.end,
        s.chars()
            .enumerate()
            .map(|(i, c)| (c, i + span.start..i + span.start + 1)),
    );

    map_second(parser.parse_recovery(s), |v| {
        v.into_iter().map(HandledError::into).collect()
    })
}

// XXX: This function is pretty complicated.
//
// We don't want to have [DirectiveParseErrorHandler] imply [string_with_vars::
// ParseErrorHandler] because there are multiple places that parsing templates
// happen, and each of those should emit a different failure reason. Instead,
// we need to emit a domain-specific error about template parsing failing
// *during directive processing*. Secondly, because we need dynamic information
// (namely, [span], but also [why]), we can't do the easy final-tagless thing
// to convert between the two (because trait impls can't capture dynamic data)
// and are instead forced to create and then destruct on this enum. There's
// probably some way to make this work out, but I don't want to spend much more
// time on it. We could probably generate the right thing with macros, but
// that's even more complicated and tricky.
//
// We don't really have to define these types as function-local, but it makes
// it clear that it's just some function-internal artifact that should go away,
// which also makes it more acceptable to name it something non-descriptive,
// like [S], which saves typing.
fn parse_command_line<E>(
    arg: Spanned<String>,
    why: Directive<NoPayload>,
) -> (Option<(RelativePathBuf, Vec<StringWithVars>)>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let (result, mut errs) = parse_and_map_errors(command_line(), arg);

    enum S {
        BadPostPercent(Span),
        BadIdentifier(Span),
        UnclosedVar(Span),
    }

    impl string_with_vars::ParseErrorHandler for S {
        fn bad_post_percent(span: Span) -> Self {
            Self::BadPostPercent(span)
        }

        fn bad_identifier(span: Span) -> Self {
            Self::BadIdentifier(span)
        }

        fn unclosed_var(span: Span) -> Self {
            Self::UnclosedVar(span)
        }
    }

    impl S {
        pub fn into<E>(self, why: &Directive<NoPayload>) -> E
        where
            E: DirectiveParseErrorHandler,
        {
            let (text, span) = match self {
            S::BadPostPercent(span) => ("% should be followed by { or %", span),
            S::BadIdentifier(span) => (
                "identifiers in %{} templates should be numbers, letters and _",
                span,
            ),
            S::UnclosedVar(span) => ("unmatched %{ in template", span),
        };

            E::error_in_template(text, why.clone(), span)
        }
    }

    // references so we don't move them into the closure
    let why = why.clone();
    let errs_ref = &mut errs;
    if let Some((prog, args)) = result {
        let args = args
            .into_iter()
            .map(|s| match StringWithVars::parse(&s, None) {
                Ok(s_with_vars) => s_with_vars,
                Err(inner_errs) => {
                    let inner_errs: Vec<S> = inner_errs;
                    for err in inner_errs.into_iter() {
                        errs_ref.push(err.into(&why))
                    }

                    StringWithVars::from(vec![string_with_vars::text(s)])
                }
            })
            .collect();

        (Some((prog, args)), errs)
    } else {
        (None, errs)
    }
}

fn parse_one_path_impl<E>(
    arg: Spanned<String>,
    why: Directive<NoPayload>,
) -> (Option<RelativePathBuf>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let parser = maybe_quoted(path())
        .labelled(Label::OnePath(why.clone()))
        .padded()
        .then_ignore(end())
        .labelled(Label::OnePath(why));

    parse_and_map_errors(parser, arg)
}

fn parse_identifier_impl<E>(
    arg: Spanned<String>,
    why: Directive<NoPayload>,
) -> (Option<String>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_and_map_errors(single_identifier(why), arg)
}

fn parse_no_arg<E>(
    arg: Spanned<String>,
    why: Directive<NoPayload>,
) -> (Option<()>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let parser = end().padded().map_err_with_span(|_, span| {
        HandledError::Known(E::no_arg_expected(why.clone(), span))
    });

    parse_and_map_errors(parser, arg)
}

/*
    type Define = (String, Definition);
    type Include = RelativePathBuf;
    type Incbin = RelativePathBuf;
    type Incext = (RelativePathBuf, Vec<StringWithVars>);
    type Inctevent = (RelativePathBuf, Vec<StringWithVars>);
    type IfDef = String;
    type IfNDef = String;
    type Else = ();
    type Endif = ();
    type Pool = ();
    type Undef = ();
*/

pub fn parse_define<E>(
    arg: Spanned<<Unparsed as directive::Args>::Define>,
) -> (Option<<Parsed as directive::Args>::Define>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let header = text::ident()
        .padded()
        .then(
            text::ident()
                .padded()
                .map_with_span(id2)
                .separated_by(just(','))
                .delimited_by(just('('), just(')'))
                .or_not(),
        )
        .padded()
        .try_map(|(name, args), _| {
            let args = match args {
                None => return Ok((name, None)),
                Some(args) => args,
            };

            let mut argset = IndexSet::new();

            for (arg, span) in args {
                let not_already_present = argset.insert(arg);

                if !not_already_present {
                    return Err(HandledError::Known(E::macro_duplicate_arg(
                        span,
                    )));
                }
            }

            Ok((name, Some(argset)))
        });

    let param = choice((
        choice((none_of("\"\\"), just('\\').ignore_then(just('"'))))
            .repeated()
            .delimited_by(just('"'), just('"')),
        none_of("\"").repeated(),
    ))
    .collect::<String>()
    .map_with_span(id2);

    let (s, errs) = parse_and_map_errors(header.then(param.or_not()), arg);

    let ((name, args), body) = match s {
        None => return (None, errs),
        Some(s) => s,
    };

    let (body, span) = match body {
        None => return (Some((name, args, Definition::Empty)), errs),
        Some(b) => b,
    };

    enum S {
        FoundHash(Span),
        UnclosedQuotes(Span),
    }

    impl GenericParseErrorHandler<char> for S {
        fn expected(
            span: Span,
            _expected: HashSet<Option<char>>,
            _found: Option<char>,
        ) -> Self {
            panic!(
                "BUG: got unlabeled parse error at {span:?} when lexing macro definition",
                span = span
            )
        }
    }

    impl LexErrorHandler for S {
        fn unclosed_comment(span: Span) -> Self {
            panic!(
                "BUG: got [unclosed_comment] at {span:?} when lexing macro definition",
                span = span
            )
        }

        fn unclosed_string_literal(span: Span) -> Self {
            S::UnclosedQuotes(span)
        }

        fn bad_directive(span: Span) -> Self {
            S::FoundHash(span)
        }
    }

    let body: Result<Vec<LexOut>, Vec<S>> = lex(body, Some(&span));
    let mut errs = errs;

    let body = match body {
        Ok(body) => body,
        Err(lex_errs) => {
            errs.append(
                &mut lex_errs
                    .into_iter()
                    .map(|e| match e {
                        S::UnclosedQuotes(span) => E::unclosed_quotes(span),
                        S::FoundHash(span) => E::macro_contains_hash(span),
                    })
                    .collect(),
            );

            vec![]
        }
    };

    let mut body: Vec<Token> = body
        .into_iter()
        .filter_map(|t| match t {
            LexOut::Token((t, _)) => Some(t),
            LexOut::Directive((_, span)) => {
                errs.push(E::macro_contains_directive(span));
                None
            }
            LexOut::Message((_, span)) => {
                errs.push(E::macro_contains_message(span));
                None
            }
        })
        .collect();

    match &body.last() {
        Some(Token::Break) => {
            (&mut body).pop();
        }
        Some(_) | None => (),
    };
    let body = body;

    (Some((name, args, Definition::Macro(body))), errs)
}

pub fn parse_include<E>(
    arg: Spanned<<Unparsed as directive::Args>::Include>,
) -> (Option<<Parsed as directive::Args>::Include>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_one_path_impl(arg, Directive::Include(()))
}

pub fn parse_incbin<E>(
    arg: Spanned<<Unparsed as directive::Args>::Incbin>,
) -> (Option<<Parsed as directive::Args>::Incbin>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_one_path_impl(arg, Directive::Incbin(()))
}

pub fn parse_ifdef<E>(
    arg: Spanned<<Unparsed as directive::Args>::IfDef>,
) -> (Option<<Parsed as directive::Args>::IfDef>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_identifier_impl(arg, Directive::IfDef(()))
}

pub fn parse_ifndef<E>(
    arg: Spanned<<Unparsed as directive::Args>::IfNDef>,
) -> (Option<<Parsed as directive::Args>::IfNDef>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_identifier_impl(arg, Directive::IfNDef(()))
}

pub fn parse_else<E>(
    arg: Spanned<<Unparsed as directive::Args>::Else>,
) -> (Option<<Parsed as directive::Args>::Else>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_no_arg(arg, Directive::Else(()))
}

pub fn parse_endif<E>(
    arg: Spanned<<Unparsed as directive::Args>::Endif>,
) -> (Option<<Parsed as directive::Args>::Endif>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_no_arg(arg, Directive::Endif(()))
}

pub fn parse_undef<E>(
    arg: Spanned<<Unparsed as directive::Args>::Undef>,
) -> (Option<<Parsed as directive::Args>::Undef>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_identifier_impl(arg, Directive::Undef(()))
}

pub fn parse_pool<E>(
    arg: Spanned<<Unparsed as directive::Args>::Pool>,
) -> (Option<<Parsed as directive::Args>::Pool>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_no_arg(arg, Directive::Pool(()))
}

pub fn parse_incext<E>(
    arg: Spanned<<Unparsed as directive::Args>::Incext>,
) -> (Option<<Parsed as directive::Args>::Incext>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_command_line(arg, Directive::Incext(()))
}

pub fn parse_inctevent<E>(
    arg: Spanned<<Unparsed as directive::Args>::Inctevent>,
) -> (Option<<Parsed as directive::Args>::Inctevent>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    parse_command_line(arg, Directive::Inctevent(()))
}
