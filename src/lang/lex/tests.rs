use super::*;
use token::Directive::*;
use token::Token::*;
use LexError::*;

fn token_match(ts: (&Token, &Token)) -> bool {
    match ts {
        (Ident(s1), Ident(s2)) => s1 == s2,
        (Number(i1), Number(i2)) => i1 == i2,
        (QuotedString(s1), QuotedString(s2)) => s1 == s2,
        (Colon, Colon) => true,
        (Dash, Dash) => true,
        (Slash, Slash) => true,
        (Star, Star) => true,
        (Plus, Plus) => true,
        (Percent, Percent) => true,
        (Ampersand, Ampersand) => true,
        (Bar, Bar) => true,
        (Caret, Caret) => true,
        (LShift, LShift) => true,
        (RShift, RShift) => true,
        (Comma, Comma) => true,
        (LCurly, LCurly) => true,
        (RCurly, RCurly) => true,
        (LParen, LParen) => true,
        (RParen, RParen) => true,
        (LBrack, LBrack) => true,
        (RBrack, RBrack) => true,
        (LAngle, LAngle) => true,
        (RAngle, RAngle) => true,
        (Break, Break) => true,
        (Semi, Semi) => true,
        (Directive(d1), Directive(d2)) => d1 == d2,
        (Filepath(p1), Filepath(p2)) => p1.as_path() == p2.as_path(),
        (Error(BadChar { chr: c1 }), Error(BadChar { chr: c2 })) => c1 == c2,
        (Error(UnclosedQuote), Error(UnclosedQuote)) => true,
        (Error(UnclosedComment), Error(UnclosedComment)) => true,
        (Error(Unescape(_)), Error(Unescape(_))) => true,
        (
            Error(InvalidBaseNum { s: s1, base: i1 }),
            Error(InvalidBaseNum { s: s2, base: i2 }),
        ) => s1 == s2 && i1 == i2,
        (Error(Overflow { n: n1 }), Error(Overflow { n: n2 })) => n1 == n2,
        (Error(BadDirective { s: s1 }), Error(BadDirective { s: s2 })) => s1 == s2,
        (Error(BadPathChar { c: c1 }), Error(BadPathChar { c: c2 })) => c1 == c2,
        _ => false,
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        token_match((self, other))
    }
}

impl Eq for Token {}

fn lexer_test(src: &str, fname: &str, v: Vec<Token>) {
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

    lexer_test(
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

    lexer_test(
        src,
        "block comment consumes line break",
        vec![Ident("UNIT".to_string()), Number(0)],
    );
}

#[test]
fn test_numbers() {
    let src = r"0x10 $10 0b10";

    lexer_test(
        src,
        "special number formatting",
        vec![Number(16), Number(16), Number(2)],
    );
}

#[test]
fn test_backslash_escaped_newline() {
    let src = r"UNIT \
    0x1";

    lexer_test(
        src,
        "backslashes escape newlines",
        vec![Ident("UNIT".to_string()), Number(1)],
    );
}

#[test]
fn test_filepaths() {
    use std::path::PathBuf;

    let src = r#"
        #include path/1/2/3
        #include path\1\2\3
        #include "path\1 2\3\4"
        1 / 2
    "#;

    let mut p1 = PathBuf::new();
    p1.push("path");
    p1.push("1");
    p1.push("2");
    p1.push("3");

    let mut p2 = PathBuf::new();
    p2.push("path");
    p2.push("1 2");
    p2.push("3");
    p2.push("4");

    lexer_test(
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
