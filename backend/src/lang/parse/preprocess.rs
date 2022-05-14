// Directive processing
use crate::{
    lang::{
        parse::{
            directive as parse_directive,
            lexer::{Out as LexOut, OutImpl as LexOutImpl},
        },
        syntax::{
            directive,
            directive::{Parsed, Unparsed},
            Directive, Span, Spanned, SpannedW,
        },
    },
    plumbing::*,
    types::{hkt::IdentityW, StringWithVars},
};

use relative_path::RelativePathBuf;

pub use parse_directive::DirectiveParseErrorHandler;

#[cfg(test)]
mod tests;

pub trait PreprocessErrorHandler {
    fn unclosed_if(source: Span) -> Self;

    fn dangling_endif(span: Span) -> Self;

    fn dangling_else(span: Span) -> Self;
}

fn tag<A, B>(
    x: (Option<A>, B),
    f: impl Fn(A) -> Directive<Parsed>,
) -> (Option<Directive<Parsed>>, B) {
    map_first(x, |x| Option::map(x, f))
}

fn parse_directive<E>(
    (d, span): Spanned<Directive<Unparsed>>,
) -> (Option<Directive<Parsed>>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    use Directive::*;

    match d {
        Define(s) => tag(parse_directive::parse_define((s, span)), Define),
        Include(p) => tag(parse_directive::parse_include((p, span)), Include),
        Incbin(p) => tag(parse_directive::parse_incbin((p, span)), Incbin),
        Incext(c) => tag(parse_directive::parse_incext((c, span)), Incext),
        Inctevent(c) => {
            tag(parse_directive::parse_inctevent((c, span)), Inctevent)
        }
        IfDef(s) => tag(parse_directive::parse_ifdef((s, span)), IfDef),
        IfNDef(s) => tag(parse_directive::parse_ifndef((s, span)), IfNDef),
        Else(s) => tag(parse_directive::parse_else((s, span)), Else),
        Endif(s) => tag(parse_directive::parse_endif((s, span)), Endif),
        Pool(s) => tag(parse_directive::parse_pool((s, span)), Pool),
        Undef(s) => tag(parse_directive::parse_undef((s, span)), Undef),
    }
}

pub type TreeItem = LexOutImpl<
    directive::Tree<
        Tree,
        RelativePathBuf,
        (RelativePathBuf, Vec<StringWithVars>),
    >,
    SpannedW,
    IdentityW,
>;

// See [directive::Tree] for an explanation of this type.
#[derive(Debug, PartialEq, Eq)]
pub struct Tree(Vec<TreeItem>);

type LexOutParsed = LexOutImpl<Parsed, SpannedW, IdentityW>;

// FIXME: Going through this representation requires a full pass over all
// tokens. If we end up having performance problems, we should re-examine
// whether we want to perform some stream fusion or just skip it entirely by
// moving [parse::directive] into [lexer].
fn to_parsed<E>(
    raw: impl Iterator<Item = LexOut>,
) -> (Vec<LexOutParsed>, Vec<E>)
where
    E: DirectiveParseErrorHandler,
{
    let mut errs = vec![];

    let result = raw
        .filter_map(|item| match item {
            LexOut::Token(t) => Some(LexOutParsed::Token(t)),
            LexOut::Message(m) => Some(LexOutParsed::Message(m)),
            LexOut::Directive((d, span)) => {
                let (d, mut errs_) = parse_directive((d, span.clone()));
                errs.append(&mut errs_);
                d.map(|d| LexOutParsed::Directive((d, span)))
            }
        })
        .collect();

    (result, errs)
}

enum YieldReason {
    Eof,
    Else(Span),
    Endif(Span),
}

enum Event {
    Yield(YieldReason),
    Item(TreeItem),
}

fn tree_item<I, E>(toks: &mut I, errs: &mut Vec<E>) -> Event
where
    I: Iterator<Item = LexOutParsed>,
    E: PreprocessErrorHandler,
{
    let item = match toks.next() {
        None => return Event::Yield(YieldReason::Eof),
        Some(item) => item,
    };

    use directive::Directive::*;
    use LexOutImpl::*;

    match item {
        Token(t) => (Event::Item(Token(t))),
        Message(m) => (Event::Item(Message(m))),
        Directive((Define(defn), span)) => {
            Event::Item(Directive((Define(defn), span)))
        }
        Directive((Include(path), span)) => {
            Event::Item(Directive((Include(path), span)))
        }
        Directive((Incext(path), span)) => {
            Event::Item(Directive((Incext(path), span)))
        }
        Directive((Incbin(prg), span)) => {
            Event::Item(Directive((Include(prg), span)))
        }
        Directive((Inctevent(prg), span)) => {
            Event::Item(Directive((Inctevent(prg), span)))
        }
        Directive((IfDef(s), span)) => {
            let (then, else_) = if_tree(toks, span.clone(), errs);
            Event::Item(Directive((IfDef((s, then, else_)), span)))
        }
        Directive((IfNDef(s), span)) => {
            let (then, else_) = if_tree(toks, span.clone(), errs);
            Event::Item(Directive((IfNDef((s, then, else_)), span)))
        }
        Directive((Undef(s), span)) => Event::Item(Directive((Undef(s), span))),
        Directive((Pool(()), span)) => Event::Item(Directive((Pool(()), span))),
        Directive((Endif(()), span)) => Event::Yield(YieldReason::Endif(span)),
        Directive((Else(()), span)) => Event::Yield(YieldReason::Else(span)),
    }
}

fn if_tree<I, E>(
    toks: &mut I,
    source: Span,
    errs: &mut Vec<E>,
) -> (Tree, Option<Tree>)
where
    I: Iterator<Item = LexOutParsed>,
    E: PreprocessErrorHandler,
{
    let (then, yield_reason) = tree(toks, errs);

    match yield_reason {
        YieldReason::Eof => errs.push(E::unclosed_if(source.clone())),
        YieldReason::Endif(_span) => return (then, None),
        YieldReason::Else(_span) => (),
    };

    let (else_, yield_reason) = tree(toks, errs);

    match yield_reason {
        YieldReason::Eof => errs.push(E::unclosed_if(source)),
        YieldReason::Endif(_span) => (),
        YieldReason::Else(span) => errs.push(E::dangling_else(span)),
    };

    return (then, Some(else_));
}

fn tree<I, E>(toks: &mut I, errs: &mut Vec<E>) -> (Tree, YieldReason)
where
    I: Iterator<Item = LexOutParsed>,
    E: PreprocessErrorHandler,
{
    let mut inner = vec![];

    loop {
        let item = tree_item(toks, errs);
        match item {
            Event::Item(item) => inner.push(item),
            Event::Yield(reason) => return (Tree(inner), reason),
        }
    }
}

// We could output [impl Iter<Item = Out>] instead of [Vec<Out>] (and the same
// for errors/warnings). However, the bookkeeping required for that is pretty
// messy (and/or requires importing a coroutine library like gen-awaiter),
// which I don't want to do unless it becomes obvious that the extra parsing
// pass is an actual performance problem.

pub fn collect_preprocess_tree<E>(
    toks: impl Iterator<Item = LexOut>,
) -> (Tree, Vec<E>)
where
    E: PreprocessErrorHandler + DirectiveParseErrorHandler,
{
    let (toks, mut errs) = to_parsed(toks);

    let (result, yield_reason) = tree(&mut toks.into_iter(), &mut errs);

    match yield_reason {
        YieldReason::Eof => (),
        YieldReason::Endif(span) => errs.push(E::dangling_endif(span)),
        YieldReason::Else(span) => errs.push(E::dangling_else(span)),
    };

    (result, errs)
}
