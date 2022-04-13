// Directive processing

use crate::{
    lang::{
        parse::{lexer, lexer::OutImpl, GenericParseErrorHandler},
        syntax::{
            directive,
            directive::{Args, Definition, Parsed, Unparsed},
            Directive, Location, Span, Spanned, SpannedW, Token,
        },
    },
    types::{hkt::IdentityW, StringWithVars},
};

use relative_path::RelativePathBuf;

pub trait DirectiveParseErrorHandler {
    fn empty_string(span: Span) -> Self;

    fn found_absolute_path(span: Span) -> Self;

    fn unclosed_string(span: Span) -> Self;

    fn empty_path(span: Span) -> Self;

    fn bad_path_char(span: Span) -> Self;

    fn empty_identifier(why: &'static str, span: Span) -> Self;

    fn invalid_identifier(why: &'static str, span: Span) -> Self;

    fn takes_no_arg(what: &'static str, span: Span) -> Self;
}

fn is_valid_path_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == ' ' || c == '.' || c == '_'
}

fn parse_path<E>(s: Spanned<String>) -> (RelativePathBuf, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let (s, span) = s;

    let s = s.trim();

    let mut errs = Vec::new();

    let mut result = RelativePathBuf::new();

    if s.is_empty() {
        errs.push(E::empty_path(span));
        return (result, errs);
    }

    let chars: Vec<(usize, char)> = s.char_indices().collect();

    let chars = if let (_, '"') = chars[0] {
        if s.len() <= 1 {
            errs.push(E::unclosed_string(span));
            return (result, errs);
        }

        if let (_, '"') = chars.last().unwrap() {
            &chars[1..chars.len() - 1]
        } else {
            errs.push(E::unclosed_string(span.clone()));
            &chars[1..]
        }
    } else {
        &chars[..]
    };

    if chars.is_empty() {
        errs.push(E::empty_path(span.clone()));
        return (result, errs);
    }

    if let (_, '/') = chars[0] {
        errs.push(E::found_absolute_path(span.clone()));
    }

    if chars.len() > 1 {
        if let (_, ':') = chars[1] {
            errs.push(E::found_absolute_path(span.clone()));
        }
    }

    let mut current_segment = String::new();

    for (idx, c) in chars {
        let (idx, c) = (*idx, *c);

        if let '/' | '\\' = c {
            result.push(current_segment);
            current_segment = String::new();
        }

        // this offset is wrong if we trimmed any whitespace
        if !is_valid_path_char(c) {
            errs.push(E::bad_path_char(Span {
                start: span.start + idx,
                end: span.start + idx + 1,
            }))
        }

        current_segment.push(c);
    }

    (result, errs)
}

fn check_valid_identifier<E>(
    s: &String,
    why: &'static str,
    span: Span,
) -> Vec<E>
where
    E: DirectiveParseErrorHandler,
{
    let s = s.trim();

    if s.is_empty() {
        return vec![E::empty_identifier(why, span)];
    }

    let s: Vec<char> = s.chars().collect();

    if !s[0].is_ascii_alphabetic() && s[0] != '_' {
        return vec![E::invalid_identifier(why, span)];
    }

    for c in s.iter() {
        if !c.is_ascii_alphanumeric() && *c != '_' {
            return vec![E::invalid_identifier(why, span)];
        }
    }

    // no errors found
    vec![]
}

fn mark_identifier_with<E, F>(
    f: F,
    s: String,
    why: &'static str,
    span: Span,
) -> (Directive<Parsed>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
    F: FnOnce(String) -> Directive<Parsed>,
{
    let is_valid = check_valid_identifier(&s, why, span);
    (f(s), is_valid)
}

fn map_fst<A, A2, B, F>(f: F, (x, v): (A, B)) -> (A2, B)
where
    F: FnOnce(A) -> A2,
{
    (f(x), v)
}

fn assert_no_arg<E>(s: &String, what: &'static str, span: Span) -> Vec<E>
where
    E: DirectiveParseErrorHandler,
{
    if s.trim().is_empty() {
        vec![]
    } else {
        vec![E::takes_no_arg(what, span)]
    }
}

fn parse_directive<E>(
    (d, span): Spanned<Directive<Unparsed>>,
) -> (Directive<Parsed>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    use Directive::*;

    match d {
        Define(_) => todo!(),
        Include(p) => map_fst(Include, parse_path((p, span))),
        Incbin(p) => map_fst(Include, parse_path((p, span))),
        Incext(_) => todo!(),
        Inctevent(_) => todo!(),
        IfDef(s) => mark_identifier_with(IfDef, s, "ifdef", span),
        IfNDef(s) => mark_identifier_with(IfNDef, s, "ifndef", span),
        Else(s) => (Else(()), assert_no_arg(&s, "else", span)),
        Endif(s) => (Endif(()), assert_no_arg(&s, "endif", span)),
        Pool(s) => (Pool(()), assert_no_arg(&s, "pool", span)),
        Undef(s) => mark_identifier_with(Undef, s, "undef", span),
    }
}

// See [directive::Tree] for an explanation of this type.
//
// FIXME: Going through this representation requires a full pass over all
// tokens. If we end up having performance problems, we should re-examine
// whether we want to switch to a lazy form (for stream fusion), or skipping it
// entirely.
#[derive(Debug, PartialEq, Eq)]
struct TreeUnexpanded(
    Box<
        OutImpl<
            directive::Tree<
                TreeUnexpanded,
                RelativePathBuf,
                (RelativePathBuf, StringWithVars),
            >,
            SpannedW,
            IdentityW,
        >,
    >,
);

// We could output [impl Iter<Item = Out>] instead of [Vec<Out>] (and the same
// for errors/warnings). However, the bookkeeping required for that is pretty
// messy (and/or requires importing a coroutine library like gen-awaiter),
// which I don't want to do unless it becomes obvious that the extra parsing
// pass is an actual performance problem.
