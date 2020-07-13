use super::*;
use token::Token::*;
use Definition::*;
use PreprocError::*;

fn token_match(ts: (&Token, &Token)) -> bool {
    use super::LexError::*;

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
        (Error(UnexpectedEof { hint: _ }), Error(UnexpectedEof { hint: _ })) => true,
        (Error(ExpectLParen), Error(ExpectLParen)) => true,
        (
            Error(LexError(BadChar { chr: c1 })),
            Error(LexError(BadChar { chr: c2 })),
        ) => c1 == c2,
        (Error(LexError(UnclosedQuote)), Error(LexError(UnclosedQuote))) => true,
        (Error(LexError(UnclosedComment)), Error(LexError(UnclosedComment))) => true,
        (Error(LexError(Unescape(_))), Error(LexError(Unescape(_)))) => true,
        (
            Error(LexError(InvalidBaseNum { s: s1, base: i1 })),
            Error(LexError(InvalidBaseNum { s: s2, base: i2 })),
        ) => s1 == s2 && i1 == i2,
        (Error(LexError(Overflow { n: n1 })), Error(LexError(Overflow { n: n2 }))) => {
            n1 == n2
        }
        (
            Error(LexError(BadDirective { s: s1 })),
            Error(LexError(BadDirective { s: s2 })),
        ) => s1 == s2,
        (
            Error(LexError(BadPathChar { c: c1 })),
            Error(LexError(BadPathChar { c: c2 })),
        ) => c1 == c2,
        _ => false,
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        token_match((self, other))
    }
}

impl Eq for Token {}

fn lident(s: &str) -> LexToken {
    Ident(s.to_string())
}

fn ident(s: &str) -> Token {
    Ident(s.to_string())
}

#[test]
fn test_nonrec_expand() {
    fn annot(value: LexToken) -> LexTokenAnnot {
        FilePosAnnot {
            value,
            row: 0,
            col: 0,
            fname: "basic expansion test".to_string(),
        }
    }

    let stream: Vec<_> = vec![
        lident("a"),
        lident("b"),
        lident("c"),
        lident("d"),
        LParen,
        lident("e"),
        Comma,
        lident("f"),
        RParen,
    ]
    .into_iter()
    .map(annot)
    .collect();

    let ctx: HashMap<String, Definition> = vec![
        ("b".to_string(), Empty),
        (
            "c".to_string(),
            Rename(vec![ident("g"), ident("h")].into_iter().collect()),
        ),
        (
            "d".to_string(),
            Macro(
                vec!["a".to_string(), "b".to_string()].into_iter().collect(),
                vec![ident("b"), ident("a")].into_iter().collect(),
            ),
        ),
    ]
    .into_iter()
    .collect();

    let p: Preprocessor<std::vec::IntoIter<LexTokenAnnot>> = Preprocessor {
        stream: stream.into_iter(),
        buffer: Vec::new(),
        ifstack: Vec::new(),
        defines: ctx,
        expanded: IndexSet::new(),
    };

    assert_eq!(
        p.map(FilePosAnnot::extract_value).collect::<Vec<Token>>(),
        vec![ident("a"), ident("g"), ident("h"), ident("f"), ident("e"),]
    );
}
