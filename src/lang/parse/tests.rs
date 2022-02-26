use std::collections::HashSet;

use super::*;

impl GenericParseErrorHandler<char> for LexError {
    fn expected(
        span: Span,
        expected: HashSet<Option<char>>,
        found: Option<char>,
    ) -> Self {
        Self::Unlabeled(span, expected, found)
    }

    fn unclosed_delimiter(
        span: Span,
        _: char,
        expected: char,
        found: Option<char>,
    ) -> Self {
        let mut set = HashSet::new();

        set.insert(Some(expected));

        Self::Unlabeled(span, set, found)
    }
}

fn uncarrier<I, E>(errors: Vec<Carrier<I, E>>) -> Vec<E>
where
    E: GenericParseErrorHandler<I>,
{
    errors
        .into_iter()
        .map(|item| {
            use Carrier::*;
            match item {
                GenericParseError {
                    span,
                    expected,
                    found,
                } => E::expected(span, expected, found),
                GenericUnclosedDelimiter {
                    unclosed_span: _,
                    unclosed,
                    span,
                    expected,
                    found,
                } => E::unclosed_delimiter(span, unclosed, expected, found),
                Specific(e) => e,
            }
        })
        .collect()
}

fn lex<O>(
    parser: impl Parser<char, O, Error = Carrier<char, LexError>>,
    text: &'static str,
) -> Result<O, Vec<LexError>> {
    parser.parse(text).map_err(uncarrier)
}

#[derive(Debug, PartialEq, Eq)]
enum LexError {
    Unlabeled(Span, HashSet<Option<char>>, Option<char>),
    UnclosedComment,
}

impl LexErrorHandler for LexError {
    fn unclosed_comment(span: Span) -> Self {
        Self::UnclosedComment
    }
}

#[test]
fn test_line_comments() {
    let line_comment = super::line_comment::<LexError>().then_ignore(end());

    let basic = r#"// this is a line comment
    "#;

    assert_eq!(lex(line_comment.clone(), basic), Ok(()));

    let eof = r#"// this is a line comment"#;

    assert_eq!(lex(line_comment.clone(), eof), Ok(()));

    let escaped = r#"// this is a line comment \
                  that is escaped"#;

    assert_eq!(lex(line_comment.clone(), escaped), Ok(()));
}

#[test]
fn test_block_comments() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let basic = r#"/* comment
        */"#;

    assert_eq!(lex(block_comment.clone(), basic), Ok(()));

    let nested = r#"/* a /* b */ c */"#;

    assert_eq!(lex(block_comment.clone(), nested), Ok(()));

    let with_line = r#"/*
      // this line is commented */
      */
      "#;

    assert_eq!(lex(block_comment.clone(), with_line), Ok(()));

    let unclosed = r#"/*"#;

    assert_eq!(
        lex(block_comment.clone(), unclosed),
        Err(vec![LexError::UnclosedComment])
    );
}

#[test]
fn edge_cases() {
    // vanilla EA does not emit a newline here
    let mid_line_block_comment = r#"A /*
    */ B"#;
}
