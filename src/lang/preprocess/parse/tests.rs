use relative_path::RelativePathBuf;

use super::*;

#[derive(Debug, PartialEq, Eq)]
enum Stmt {
    Tokens(Vec<Token>),
    IfDef(String, Vec<Stmt>, Vec<Stmt>),
    Include(RelativePathBuf),
    Incbin(RelativePathBuf),
    Pool,
    Define(String, Definition),
    Undef(String),
    Malformed,
    Incext(RelativePathBuf, Vec<Token>),
    Inctevent(RelativePathBuf, Vec<Token>),
}

fn strip_positions(t: Ast) -> Vec<Stmt> {
    t.into_iter()
        .map(|node| match node {
            Statement::Tokens(t) => {
                Stmt::Tokens(t.into_iter().map(FilePosAnnot::extract_value).collect())
            }
            Statement::IfDef(s, t1, t2) => {
                Stmt::IfDef(s, strip_positions(t1), strip_positions(t2))
            }
            Statement::Include(p) => Stmt::Include(p),
            Statement::Incbin(p) => Stmt::Incbin(p),
            Statement::Pool => Stmt::Pool,
            Statement::Define(s, d) => Stmt::Define(s, d),
            Statement::Undef(s) => Stmt::Undef(s),
            Statement::Malformed => Stmt::Malformed,
            Statement::Incext(p, ts) => Stmt::Incext(p, ts),
            Statement::Inctevent(p, ts) => Stmt::Inctevent(p, ts),
        })
        .collect()
}

fn tokens_only(src: &str, fname: &str, t: Vec<Stmt>) {
    let stream = src.chars();
    let mut lexer = lex(fname.to_string(), stream);
    let result = strip_positions(ast(&mut lexer).extract().0);

    assert_eq!(result, t);
}

use Stmt::*;

#[test]
fn test_basic() {
    let src = r#"
    a b
    e f
    #include a/b/c

    #ifdef a
        b
    #endif

    #ifndef c
        #ifdef d
            e
        #else
            f
        #endif
    #endif
    "#;

    let mut p = RelativePathBuf::new();
    p.push("a");
    p.push("b");
    p.push("c");

    tokens_only(
        src,
        "basic test",
        vec![
            Tokens(vec![Ident("a".to_string()), Ident("b".to_string())]),
            Tokens(vec![Ident("e".to_string()), Ident("f".to_string())]),
            Include(p),
            IfDef(
                "a".to_string(),
                vec![Tokens(vec![Ident("b".to_string())])],
                vec![],
            ),
            IfDef(
                "c".to_string(),
                vec![],
                vec![IfDef(
                    "d".to_string(),
                    vec![Tokens(vec![Ident("e".to_string())])],
                    vec![Tokens(vec![Ident("f".to_string())])],
                )],
            ),
        ],
    );
}
