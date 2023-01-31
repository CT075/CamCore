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

fn display_option<I>(v: Option<I>) -> String
where
    I: std::fmt::Display,
{
    match v {
        None => "end of file".to_string(),
        Some(v) => format!("{}", v),
    }
}

fn format_expected_set<I>(s: HashSet<Option<I>>) -> String
where
    I: std::fmt::Display + std::hash::Hash + Eq,
{
    if s.len() <= 0 {
        return ".".to_string();
    }
    if s.len() == 1 {
        for v in s.into_iter() {
            match v {
                None => return ", expected end of file.".to_string(),
                Some(v) => return format!(", expected {}", v),
            }
        }
        panic!(
            "[format_expected_set] loop ran zero times in case [s.len() == 1]"
        )
    } else {
        let contains_none = s.get(&None).is_some();

        let mut vs = s
            .into_iter()
            .filter_map(|v| v.map(|v| format!(r#""{}""#, v)))
            .collect::<Vec<_>>()
            .join(", ");

        if contains_none {
            vs.push_str(", or end of file")
        }

        return format!(", expected one of {}.", vs);
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
            .with_message(format!(
                "Syntax error: found {}{}",
                display_option(found),
                format_expected_set(expected)
            ))
            .with_label(Label::new(S(span.clone())))
            .finish();

        R(r)
    }
}
