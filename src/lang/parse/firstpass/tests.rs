use std::collections::HashSet;

use super::{super::GenericParseErrorHandler, *};

#[derive(Clone, Debug, Eq, PartialEq)]
enum OutStripped {
    Token(Token),
    Directive(Directive),
    Message(String),
}

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

fn run_parser<O>(
    parser: impl Parser<char, O, Error = Carrier<char, LexError>>,
    text: &'static str,
) -> Result<O, Vec<LexError>> {
    parser
        .parse(text)
        .map_err(|errs| errs.into_iter().map(Carrier::into).collect())
}

#[derive(Debug, PartialEq, Eq)]
enum LexError {
    Unlabeled(Span, HashSet<Option<char>>, Option<char>),
    UnclosedComment,
    UnclosedString,
}

fn lex_no_loc<'a>(
    text: &'static str,
) -> Result<Vec<OutStripped>, Vec<LexError>> {
    super::parse(text).map(|out_v| {
        out_v
            .into_iter()
            .map(|out| match out {
                Out::Token(WithLocation { value, .. }) => {
                    OutStripped::Token(value)
                }
                Out::Directive(WithLocation { value, .. }) => {
                    OutStripped::Directive(value)
                }
                Out::Message(m) => OutStripped::Message(m),
            })
            .collect()
    })
}

impl LexErrorHandler for LexError {
    fn unclosed_comment(_span: Span) -> Self {
        Self::UnclosedComment
    }

    fn unclosed_string_literal(_span: Span) -> Self {
        Self::UnclosedString
    }
}

#[test]
fn line_comment_basic() {
    let line_comment = super::line_comment::<LexError>().then_ignore(end());

    let basic = r#"// this is a line comment
    "#;

    assert_eq!(run_parser(line_comment.clone(), basic), Ok(()));
}

#[test]
fn line_comment_eof() {
    let line_comment = super::line_comment::<LexError>().then_ignore(end());

    let eof = r#"// this is a line comment"#;

    assert_eq!(run_parser(line_comment.clone(), eof), Ok(()));
}

#[test]
fn line_comment_escaped() {
    let line_comment = super::line_comment::<LexError>().then_ignore(end());

    let escaped = r#"// this is a line comment \
                  that is escaped"#;

    assert_eq!(run_parser(line_comment.clone(), escaped), Ok(()));
}

#[test]
fn block_comment_basic() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let basic = r#"/* comment
        */"#;

    assert_eq!(run_parser(block_comment.clone(), basic), Ok(()));
}

#[test]
fn block_comment_no_contents() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let basic = r#"/**/"#;

    assert_eq!(run_parser(block_comment.clone(), basic), Ok(()));
}

#[test]
fn block_comment_nested() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let nested = r#"/* a /* b */ c */"#;

    assert_eq!(run_parser(block_comment.clone(), nested), Ok(()));
}

#[test]
fn block_comment_containing_line_comment() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let with_line = r#"/*
      // this line is commented */
      */
      "#;

    assert_eq!(run_parser(block_comment.clone(), with_line), Ok(()));
}

#[test]
fn block_comment_basic_unclosed() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let unclosed = r#"/*"#;

    assert_eq!(
        run_parser(block_comment.clone(), unclosed),
        Err(vec![LexError::UnclosedComment])
    );
}

#[test]
fn block_comment_nested_unclosed() {
    let block_comment = super::block_comment::<LexError>().then_ignore(end());

    let nested = r#"/* /* */"#;

    assert_eq!(
        run_parser(block_comment.clone(), nested),
        Err(vec![LexError::UnclosedComment])
    );
}

#[test]
fn decimal() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"1234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234".to_string(), 10)))
}

#[test]
fn hex_with_0x() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"0x01234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("01234".to_string(), 16)))
}

#[test]
fn hex_with_dollar() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"$1234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234".to_string(), 16)))
}

#[test]
fn binary_lowercase() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"0110110b"#;

    assert_eq!(
        run_parser(number.clone(), n),
        Ok(("0110110".to_string(), 2))
    )
}

#[test]
fn binary_uppercase() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"0110110B"#;

    assert_eq!(
        run_parser(number.clone(), n),
        Ok(("0110110".to_string(), 2))
    )
}

#[test]
fn bad_decimal() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"1234a"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234a".to_string(), 10)))
}

#[test]
fn bad_hex() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"0x1234Z"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234Z".to_string(), 16)))
}

#[test]
fn bad_binary() {
    let number = super::number::<LexError>().then_ignore(end());

    let n = r#"311111b"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("311111".to_string(), 2)))
}

#[test]
fn quoted_string_basic() {
    let string = super::quoted_string::<LexError>().then_ignore(end());

    let s = r#""abcde""#;

    assert_eq!(run_parser(string.clone(), s), Ok("abcde".to_string()))
}

#[test]
fn quoted_string_escapes() {
    let string = super::quoted_string::<LexError>().then_ignore(end());

    let s = r#""\"\\""#;

    assert_eq!(run_parser(string.clone(), s), Ok("\"\\".to_string()))
}

#[test]
fn quoted_string_unclosed() {
    let string = super::quoted_string::<LexError>().then_ignore(end());

    let s = r#""not closed"#;

    assert_eq!(
        run_parser(string.clone(), s),
        Err(vec![LexError::UnclosedString])
    )
}

#[test]
fn dont_lex_line_comment() {
    let s = r#"/ / // this is not part of the line"#;

    use super::Token::{Break, Slash};
    use OutStripped::Token;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![Token(Slash), Token(Slash), Token(Break),])
    )
}

#[test]
fn star_slash_tricky() {
    let s = r#"/**/ / */**/ * /"#;

    use super::Token::{Break, Slash, Star};
    use OutStripped::Token;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![
            Token(Slash),
            Token(Star),
            Token(Star),
            Token(Slash),
            Token(Break)
        ])
    )
}

#[test]
fn message_with_comments() {
    let s = r#"MESSAGE this message /* contains */ // comments"#;

    use OutStripped::Message;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![Message(
            "this message /* contains */ // comments".to_string()
        )])
    )
}

#[test]
fn edge_cases() {
    // vanilla EA does not emit a newline here
    let mid_line_block_comment = r#"A /*
    */ B"#;
}
