use super::*;
use token::Directive::*;
use token::Token::*;
use LexError::*;

fn tokens_only(src: &str, fname: &str, v: Vec<Token>) {
    let stream = src.chars();
    let result: Vec<Token> = lex(fname.to_string(), stream)
        .into_iter()
        .map(FilePosAnnot::extract_value)
        .collect();

    assert_eq!(result, v);
}

#[test]
fn test_basic() {
    let src = r"
        /* this will be ignored */
        UNIT 0x0 0 (0+1) // this will also be ignored
        ENDA
    ";

    tokens_only(
        src,
        "basic test",
        vec![
            Break,
            Break,
            Ident("UNIT".to_string()),
            Number(0),
            Number(0),
            LParen,
            Number(0),
            Plus,
            Number(1),
            RParen,
            Break,
            Ident("ENDA".to_string()),
            Break,
        ],
    );
}

#[test]
fn test_block_comment_no_break() {
    let src = r"UNIT /*
      some stuff in here
    */ 0";

    tokens_only(
        src,
        "block comment consumes line break",
        vec![Ident("UNIT".to_string()), Number(0)],
    );
}

#[test]
fn test_nested_comments() {
    let src = r"UNIT /* /*
      some stuff in here
    */ 0
    */ 0";

    tokens_only(
        src,
        "block comment consumes line break",
        vec![Ident("UNIT".to_string()), Number(0)],
    );

    let src2 = r"UNIT /*
      some stuff in here
    // */ 0
    */ 1";

    tokens_only(
        src2,
        "nested comments work properly",
        vec![Ident("UNIT".to_string()), Number(1)],
    );
}

#[test]
fn test_buffered_chars() {
    let src = r"1>2 1*2 1>>2 1*>>2 1*/2";

    tokens_only(
        src,
        "multi-character operators are buffered properly",
        vec![
            Number(1),
            RAngle,
            Number(2),
            Number(1),
            Star,
            Number(2),
            Number(1),
            RShift,
            Number(2),
            Number(1),
            Star,
            RShift,
            Number(2),
            Number(1),
            Error(UnmatchedBlockClose),
            Number(2),
        ],
    );
}

#[test]
fn test_numbers() {
    let src = r"0x10 $10 0b10";

    tokens_only(
        src,
        "special number formatting",
        vec![Number(16), Number(16), Number(2)],
    );
}

#[test]
fn test_backslash_escaped_newline() {
    let src = r"UNIT \
    0x1";

    tokens_only(
        src,
        "backslashes escape newlines",
        vec![Ident("UNIT".to_string()), Number(1)],
    );
}

#[test]
fn test_filepaths() {
    use relative_path::RelativePathBuf;

    let src = r#"
        #include path/1/2/3
        #include path\1\2\3
        #include "path\1 2\3\4"
        1 / 2
    "#;

    let mut p1 = RelativePathBuf::new();
    p1.push("path");
    p1.push("1");
    p1.push("2");
    p1.push("3");

    let mut p2 = RelativePathBuf::new();
    p2.push("path");
    p2.push("1 2");
    p2.push("3");
    p2.push("4");

    tokens_only(
        src,
        "filepaths are processed correctly",
        vec![
            Break,
            Directive(Include),
            Filepath(p1.clone()),
            Break,
            Directive(Include),
            Filepath(p1.clone()),
            Break,
            Directive(Include),
            Filepath(p2),
            Break,
            Number(1),
            Slash,
            Number(2),
            Break,
        ],
    );
}
