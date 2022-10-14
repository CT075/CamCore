use std::collections::HashSet;

use ariadne::{sources, ColorGenerator, Fmt, Label, Report, ReportKind};

use ea::{
    errors::GenericParseErrorHandler,
    lang::syntax::{span::Source, Span},
};

// It's a bit unfortunate that we have to use newtypes here to avoid the
// orphan rule, but it's the price we pay for keeping the internals separate
struct S(Span);
struct R(Report<S>);

impl ariadne::Span for S {
    type SourceId = Source;

    fn source(&self) -> &Self::SourceId {
        let S(Span { source, span: _ }) = self;

        source
    }

    fn start(&self) -> usize {
        let S(Span { source: _, span }) = self;

        span.start.offset
    }

    fn end(&self) -> usize {
        let S(Span { source: _, span }) = self;

        span.end.offset
    }
}

impl R {
    pub fn into(R(r): Self) -> Report<S> {
        r
    }
}

impl<I> GenericParseErrorHandler<I> for R
where
    I: std::fmt::Display + std::hash::Hash + Eq,
{
    fn expected(
        span: Span,
        expected: HashSet<Option<I>>,
        found: Option<I>,
    ) -> Self {
        let r = Report::build(ReportKind::Error, span.source().clone(), 0)
            .with_message("Syntax error: Got unexpected input")
            .with_label(Label::new(S(span.clone())))
            .finish();

        R(r)
    }
}
