use std::collections::HashMap;

use crate::lang::syntax::{Span, Spanned};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Text(String),
    Var(Spanned<String>),
}

pub fn text(s: String) -> Element {
    Element::Text(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct S(Vec<Element>);

pub use parser::ParseErrorHandler;

pub trait RenderErrorHandler {
    fn unknown_var(v: String, span: Span) -> Self;
}

impl S {
    pub fn render<E>(
        self,
        context: &HashMap<String, String>,
    ) -> Result<String, Vec<E>>
    where
        E: RenderErrorHandler,
    {
        let mut out = String::new();

        self.render_into(context, &mut out).map(|_| out)
    }

    pub fn render_into<'a, 'b, E>(
        self,
        context: &'a HashMap<String, String>,
        out: &'b mut String,
    ) -> Result<(), Vec<E>>
    where
        E: RenderErrorHandler,
    {
        let mut errors = Vec::new();

        for item in self.0 {
            match item {
                Element::Text(s) => out.push_str(s.as_ref()),
                Element::Var((v, span)) => match context.get(&v) {
                    Some(val) => out.push_str(val.as_ref()),
                    None => errors.push(E::unknown_var(v.clone(), span)),
                },
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }
}

impl From<Vec<Element>> for S {
    fn from(v: Vec<Element>) -> Self {
        S(v)
    }
}

mod parser {
    use chumsky::{error::Simple, prelude::*};

    use crate::lang::syntax::span::{stream_spanned, Position, Source};

    use super::{Element, Span};

    pub trait ParseErrorHandler {
        fn bad_post_percent(span: Span) -> Self;

        fn bad_identifier(span: Span) -> Self;

        fn unclosed_var(span: Span) -> Self;
    }

    // TODO: The use of [Simple] here is a bit hacky. We can't use
    // [syntax::Carrier] because of the canonicity check (because we'd have two
    // blanket impls for Carrier<char>), so we have to check the label post-hoc
    // instead.
    fn parser() -> impl Parser<char, super::S, Error = Simple<char, Span>> {
        let non_escape = none_of("%");
        let literal_percent = just("%%").to('%');

        let text = non_escape
            .or(literal_percent)
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(Element::Text);

        let var = just("%{")
            .ignore_then(text::ident())
            .then_ignore(
                just("}")
                    .recover_with(skip_then_retry_until(['}']))
                    .labelled("ident"),
            )
            .labelled("end")
            .map_with_span(|v, span| Element::Var((v, span)));

        text.or(var).labelled("start").repeated().map(super::S)
    }

    impl super::S {
        pub fn parse<E>(
            source: Option<&Source>,
            start: Option<&Position>,
            s: impl AsRef<str>,
        ) -> Result<Self, Vec<E>>
        where
            E: ParseErrorHandler,
        {
            let stream = stream_spanned(source, start, s);

            parser().then_ignore(end()).parse(stream).map_err(|v| {
                v.into_iter()
                    .map(|e| match e.label() {
                        Some("start") => {
                            E::bad_post_percent(e.span())
                        }
                        Some("ident") => {
                            E::bad_identifier(e.span())
                        }
                        Some("end") => E::unclosed_var(e.span()),
                        Some(_) | None => {
                            panic!("BUG: template parsing produced unlabelled error")
                        }
                    })
                    .collect()
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::syntax::{
        span::{Position, Source},
        Span,
    };

    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    enum E {
        BadPostPercent,
        BadIdentifier,
        UnclosedVar,
        UnknownVar(String),
    }

    impl ParseErrorHandler for E {
        fn bad_post_percent(_span: Span) -> Self {
            Self::BadPostPercent
        }

        fn bad_identifier(_span: Span) -> Self {
            Self::BadIdentifier
        }

        fn unclosed_var(_span: Span) -> Self {
            Self::UnclosedVar
        }
    }

    impl RenderErrorHandler for E {
        fn unknown_var(var: String, _span: Span) -> Self {
            Self::UnknownVar(var)
        }
    }

    fn parse(s: &'static str) -> Result<super::S, Vec<E>> {
        S::parse(None, None, s)
    }

    #[test]
    fn parse_basic() {
        let s = r#"this is some text with a %{var} or two, and some %%{text}"#;

        let result: Result<_, Vec<E>> = parse(s);

        let span = Span {
            source: Source::new("_unknown_"),
            span: Position {
                offset: 25,
                row: 0,
                col: 25,
            }..Position {
                offset: 31,
                row: 0,
                col: 31,
            },
        };

        assert_eq!(
            result,
            Ok(super::S(vec![
                Element::Text("this is some text with a ".to_string()),
                Element::Var(("var".to_string(), span)),
                Element::Text(" or two, and some %{text}".to_string())
            ]))
        )
    }

    #[test]
    fn error_bad_percent() {
        let s = r#"%a"#;

        let result: Result<_, Vec<E>> = parse(s);

        assert_eq!(result, Err(vec![E::BadPostPercent]))
    }

    #[test]
    fn error_bad_ident() {
        let s = r#"%{not an identifier}"#;

        let result: Result<_, Vec<E>> = parse(s);

        assert_eq!(result, Err(vec![E::BadIdentifier]))
    }

    #[test]
    fn error_unclosed() {
        let s = r#"%{"#;

        let result: Result<_, Vec<E>> = parse(s);

        assert_eq!(result, Err(vec![E::UnclosedVar]))
    }

    #[test]
    fn render_basic() -> Result<(), Vec<E>> {
        let s = r#"hello, %{world}!"#;

        let s = parse(s)?;

        let result = s.render(
            &vec![("world".to_string(), "world".to_string())]
                .into_iter()
                .collect(),
        )?;

        assert_eq!(result, "hello, world!".to_string());

        Ok(())
    }
}
