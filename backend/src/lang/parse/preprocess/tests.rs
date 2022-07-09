use std::collections::HashSet;

use super::{super::GenericParseErrorHandler, *};

#[derive(Debug, PartialEq, Eq)]
enum Error {
    Unlabeled(Span, HashSet<Option<char>>, Option<char>),
    UnclosedComment(Span),
    UnclosedQuotes(Span),
    BadDirective(Span),
    ExpectedEndOfLine(&'static str, Span),
    UnclosedIf(&'static str, Span),
    DefineDuplicateArg(Span),
    DefineUnmatchedStartQuote(Span),
    DefineUnmatchedEndQuote(Span),
    UnmatchedElse(Span),
    UnmatchedEndif(Span),
    BadPostPercent(Span),
    BadIdentifier(Span),
    UnclosedVar(Span),
    UnmatchedParen(Span),
}

impl GenericParseErrorHandler<char> for Error {
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

impl string_with_vars::ParseErrorHandler for Error {
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

impl PpSyntaxErrorHandler for Error {
    fn unclosed_comment(span: Span) -> Self {
        Self::UnclosedComment(span)
    }

    fn unclosed_quotes(span: Span) -> Self {
        Self::UnclosedQuotes(span)
    }

    fn bad_directive(span: Span) -> Self {
        Self::BadDirective(span)
    }

    fn expected_end_of_line(why: &'static str, span: Span) -> Self {
        Self::ExpectedEndOfLine(why, span)
    }

    fn unclosed_if(which: &'static str, span: Span) -> Self {
        Self::UnclosedIf(which, span)
    }

    fn define_duplicate_arg(span: Span) -> Self {
        Self::DefineDuplicateArg(span)
    }

    fn define_unmatched_start_quote(span: Span) -> Self {
        Self::DefineUnmatchedStartQuote(span)
    }

    fn define_unmatched_end_quote(span: Span) -> Self {
        Self::DefineUnmatchedEndQuote(span)
    }

    fn unmatched_else(span: Span) -> Self {
        Self::UnmatchedElse(span)
    }

    fn unmatched_endif(span: Span) -> Self {
        Self::UnmatchedEndif(span)
    }

    fn unmatched_paren(span: Span) -> Self {
        Self::UnmatchedParen(span)
    }
}

fn run_parser<O>(
    parser: impl Parser<char, O, Error = Carrier<char, Error>>,
    text: &'static str,
) -> Result<O, Vec<Error>> {
    parser
        .parse(text)
        .map_err(|errs| errs.into_iter().map(Carrier::into).collect())
}

#[test]
fn line_comment_eof() {
    let line_comment = super::line_comment::<Error>().then_ignore(end());

    let eof = r#"// this is a line comment"#;

    assert_eq!(run_parser(line_comment.clone(), eof), Ok(()));
}

#[test]
fn line_comment_escaped() {
    let line_comment = super::line_comment::<Error>().then_ignore(end());

    let escaped = r#"// this is a line comment \
                  that is escaped"#;

    assert_eq!(run_parser(line_comment.clone(), escaped), Ok(()));
}

#[test]
fn block_comment_basic() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let basic = r#"/* comment
        */"#;

    assert_eq!(run_parser(block_comment.clone(), basic), Ok(()));
}

#[test]
fn block_comment_no_contents() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let basic = r#"/**/"#;

    assert_eq!(run_parser(block_comment.clone(), basic), Ok(()));
}

#[test]
fn block_comment_nested() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let nested = r#"/* a /* b */ c */"#;

    assert_eq!(run_parser(block_comment.clone(), nested), Ok(()));
}

#[test]
fn block_comment_containing_line_comment() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let with_line = r#"/*
      // this line is commented */
      */"#;

    assert_eq!(run_parser(block_comment.clone(), with_line), Ok(()));
}

#[test]
fn block_comment_basic_unclosed() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let unclosed = r#"/*"#;

    assert_eq!(
        run_parser(block_comment.clone(), unclosed),
        Err(vec![Error::UnclosedComment(0..2)])
    );
}

#[test]
fn block_comment_nested_unclosed() {
    let block_comment = super::block_comment::<Error>().then_ignore(end());

    let nested = r#"/* /* */"#;

    assert_eq!(
        run_parser(block_comment.clone(), nested),
        Err(vec![Error::UnclosedComment(0..2)])
    );
}

#[test]
fn decimal() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"1234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234".to_string(), 10)))
}

#[test]
fn hex_with_0x() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"0x01234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("01234".to_string(), 16)))
}

#[test]
fn hex_with_dollar() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"$1234"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234".to_string(), 16)))
}

#[test]
fn binary_lowercase() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"0110110b"#;

    assert_eq!(
        run_parser(number.clone(), n),
        Ok(("0110110".to_string(), 2))
    )
}

#[test]
fn binary_uppercase() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"0110110B"#;

    assert_eq!(
        run_parser(number.clone(), n),
        Ok(("0110110".to_string(), 2))
    )
}

#[test]
fn bad_decimal() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"1234a"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234a".to_string(), 10)))
}

#[test]
fn bad_hex() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"0x1234Z"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("1234Z".to_string(), 16)))
}

#[test]
fn bad_binary() {
    let number = super::number::<Error>().then_ignore(end());

    let n = r#"311111b"#;

    assert_eq!(run_parser(number.clone(), n), Ok(("311111".to_string(), 2)))
}

#[test]
fn quoted_string_basic() {
    let string = super::quoted_string::<Error>().then_ignore(end());

    let s = r#""abcde""#;

    assert_eq!(run_parser(string.clone(), s), Ok("abcde".to_string()))
}

#[test]
fn quoted_string_escapes() {
    let string = super::quoted_string::<Error>().then_ignore(end());

    let s = r#""\"\\""#;

    assert_eq!(run_parser(string.clone(), s), Ok("\"\\".to_string()))
}

#[test]
fn quoted_string_unclosed() {
    let string = super::quoted_string::<Error>().then_ignore(end());

    let s = r#""not closed"#;

    assert_eq!(
        run_parser(string.clone(), s),
        Err(vec![Error::UnclosedQuotes(0..1)])
    )
}

fn parse(s: &'static str) -> Result<Tree, Vec<Error>> {
    super::parse(s)
}

#[test]
fn dont_lex_line_comment() {
    insta::assert_debug_snapshot!(parse(
        r#"/ / // this is not part of the line"#
    ));
}

#[test]
fn star_slash_tricky() {
    insta::assert_debug_snapshot!(parse(
        r#"/**/ / */**/ * / /// this is a comment"#
    ));
}

// I think both vanilla EA and ColorzCore will strip the comments from a
// MESSAGE, so we should as well.
#[test]
fn message_with_comments() {
    insta::assert_debug_snapshot!(parse(
        r#"MESSAGE this message / /* contains */ // comments"#
    ));
}

#[test]
fn test_include() {
    insta::assert_debug_snapshot!(parse(r#"#include A/B/C // a b c"#));
}

#[test]
fn test_define() {
    insta::assert_debug_snapshot!(parse(r#"#define A(B, C) "B C""#));

    insta::assert_debug_snapshot!(parse(r#"#define A "B C""#));

    insta::assert_debug_snapshot!(parse(r#"#define A"#));

    insta::assert_debug_snapshot!(parse(
        r#"#define/**/A(B,/**/ /**/C)/**/ "B C" /**/ // a b c"#
    ));

    insta::assert_debug_snapshot!(parse(r#"#define A(B,C)C"#));
}

#[test]
fn test_include_with_comments() {
    insta::assert_debug_snapshot!(parse(
        r#"#include /* */ /**/ A/B/C /* */ // a b c"#
    ));
}

#[test]
fn bad_directive() {
    insta::assert_debug_snapshot!(parse(r#"#notreal"#), @r###"
    Err(
        [
            BadDirective(
                1..2,
            ),
        ],
    )
    "###);
}

#[test]
fn escaped_eol_directive() {
    insta::assert_debug_snapshot!(parse(
        r#"#include \
some/text/on/a/new/line"#
    ), @r###"
    Ok(
        Tree(
            [
                (
                    Directive(
                        Include(
                            "some/text/on/a/new/line",
                        ),
                    ),
                    0..35,
                ),
            ],
        ),
    )
    "###);
}

// Vanilla EA does not emit a newline here. It might be nice to add a lint
// to this.
#[test]
fn midline_multiline_comment() {
    insta::assert_debug_snapshot!(parse(r#"A /*
    */ B"#), @r###"
    Ok(
        Tree(
            [
                (
                    Line(
                        [
                            Single(
                                (
                                    Ident(
                                        "A",
                                    ),
                                    0..1,
                                ),
                            ),
                            Single(
                                (
                                    Ident(
                                        "B",
                                    ),
                                    12..13,
                                ),
                            ),
                        ],
                    ),
                    0..14,
                ),
            ],
        ),
    )
    "###);
}

#[test]
fn multiple_lines() {
    insta::assert_debug_snapshot!(parse(
        r#"ALIGN 4
NewChName:"#), @r###"
    Ok(
        Tree(
            [
                (
                    Line(
                        [
                            Single(
                                (
                                    Ident(
                                        "ALIGN",
                                    ),
                                    0..5,
                                ),
                            ),
                            Single(
                                (
                                    Number {
                                        payload: "4",
                                        radix: 10,
                                    },
                                    6..7,
                                ),
                            ),
                        ],
                    ),
                    0..8,
                ),
                (
                    Line(
                        [
                            Single(
                                (
                                    Ident(
                                        "NewChName",
                                    ),
                                    8..17,
                                ),
                            ),
                            Single(
                                (
                                    Colon,
                                    17..18,
                                ),
                            ),
                        ],
                    ),
                    8..19,
                ),
            ],
        ),
    )
    "###);
}

#[test]
fn realistic_test() {
    insta::assert_debug_snapshot!(parse(
        r#"#ifdef DRAGON_VEINS
VeinEffect(0, FreezeAllEnemies)
#endif // DRAGON_VEINS

setText(0x160, NewChName)

MESSAGE this example is a snippet from the FE8 Skill System

ALIGN 4
NewChName:
String("Boss Rush")"#
    ));
}
