use std::collections::HashSet;

use super::{super::GenericParseErrorHandler, *};

#[derive(Clone, Debug, Eq, PartialEq)]
enum OutStripped {
    Token(Token),
    Directive(Directive<Unparsed>),
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
    BadDirective,
}

fn lex_no_loc<'a>(
    text: &'static str,
) -> Result<Vec<OutStripped>, Vec<LexError>> {
    super::lex(text, None).map(|out_v| {
        out_v
            .into_iter()
            .map(|out| match out {
                Out::Token((value, _)) => OutStripped::Token(value),
                Out::Directive((value, _)) => OutStripped::Directive(value),
                Out::Message((value, _)) => OutStripped::Message(value),
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

    fn bad_directive(_span: Span) -> Self {
        Self::BadDirective
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
      */"#;

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
    let s = r#"/**/ / */**/ * / /// this is a comment"#;

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

// I think both vanilla EA and ColorzCore will strip the comments from a
// MESSAGE, so we should as well.
#[test]
fn message_with_comments() {
    let s = r#"MESSAGE this message / /* contains */ // comments"#;

    use OutStripped::Message;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![Message("this message /".to_string())])
    )
}

#[test]
fn directive_with_comments() {
    let s = r#"#define /* */ A /* */ "B C" // D"#;

    use super::Directive::Define;
    use OutStripped::Directive;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![Directive(Define(r#"A  "B C""#.to_string()))])
    )
}

#[test]
fn bad_directive() {
    let s = r#"#notreal"#;

    assert_eq!(lex_no_loc(s), Err(vec![LexError::BadDirective]))
}

#[test]
fn escaped_eol_directive() {
    let s = r#"#include \
some text on a new line"#;

    use super::Directive::*;
    use OutStripped::Directive;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![Directive(Include(
            "some text on a new line".to_string()
        )),])
    )
}

// Vanilla EA does not emit a newline here. It might be nice to add a lint
// to this.
#[test]
fn midline_multiline_comment() {
    let s = r#"A /*
    */ B"#;

    use super::Token::{Break, Ident};
    use OutStripped::Token;
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![
            Token(Ident("A".to_string())),
            Token(Ident("B".to_string())),
            Token(Break)
        ])
    )
}

#[test]
fn realistic_test() {
    let s = r#"#ifdef DRAGON_VEINS
VeinEffect(0, FreezeAllEnemies)
#endif // DRAGON_VEINS

setText(0x160, NewChName)

MESSAGE this example is a snippet from the FE8 Skill System

ALIGN 4
NewChName:
String("Boss Rush")"#;

    use super::{Directive::*, Token::*};
    use OutStripped::{Directive, Message, Token};
    assert_eq!(
        lex_no_loc(s),
        Ok(vec![
            Directive(IfDef("DRAGON_VEINS".to_string())),
            Token(Ident("VeinEffect".to_string())),
            Token(LParen),
            Token(Number {
                payload: "0".to_string(),
                radix: 10
            }),
            Token(Comma),
            Token(Ident("FreezeAllEnemies".to_string())),
            Token(RParen),
            Token(Break),
            Directive(Endif("".to_string())),
            Token(Ident("setText".to_string())),
            Token(LParen),
            Token(Number {
                payload: "160".to_string(),
                radix: 16
            }),
            Token(Comma),
            Token(Ident("NewChName".to_string())),
            Token(RParen),
            Token(Break),
            Message(
                "this example is a snippet from the FE8 Skill System"
                    .to_string()
            ),
            Token(Ident("ALIGN".to_string())),
            Token(Number {
                payload: "4".to_string(),
                radix: 10
            }),
            Token(Ident("NewChName".to_string())),
            Token(Colon),
            Token(Break),
            Token(Ident("String".to_string())),
            Token(LParen),
            Token(QuotedString("Boss Rush".to_string())),
            Token(RParen),
            Token(Break)
        ])
    )
}
