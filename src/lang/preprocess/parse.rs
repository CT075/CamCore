// statement. This can lead to some unfortunate error dropping, such as if
// there are multiple lexing errors on the same line, or a lexing error in
// place of an identifier.
//
// To a large extent, this is due to a misunderstanding of a paper L read about
// error recovery in parsers, in which they suggested collecting errors along
// with an error node in your syntax tree -- L had interpreted this to mean that
// the error node *itself* would contain the error context. This can lead to
// some inevitable short-circuiting in the way this parser is constructed, as
// L don't currently have an ergonomic way to produce multiple `Statement`s at
// once, which is an issue if, in the process of parsing what appears to be a
// single `Statement`, multiple errors are encountered. The simplest band-aid
// fix is to change several of the functions in this file to return a
// `Vec<Statement>` instead of just a `Statement` (for the functional nerds
// reading this, we're lifting the parser into the list monad), but L think it
// would ultimately be better to change them to return `(Statement, Vec<Error>)`
// (lift into the writer monad instead).
//
// L think the biggest impact of this error will be in cases where a malformed
// directive parameter (say, a missing name in a `#define`) is followed by
// extra junk, which will produce some weird error output. Most other edge
// cases are related to short-circuiting on lexing errors, and L don't expect
// those to be particularly common.

use std::collections::VecDeque;

use indexmap::set::IndexSet;
use relative_path::RelativePathBuf;

use super::{
    super::lex::{lex, LexError, Lexer, Token, TokenAnnot},
    syntax::{Ast, Definition, Statement},
    token,
    token::{Directive, FilePosAnnot},
    PreprocError,
};

use token::Token::*;

#[cfg(test)]
mod tests;

macro_rules! expect {
    ( $stream:expr, $p:pat, $row:pat, $col:pat, $b:block, $err:expr, $s:expr ) => {{
        match $stream.next() {
            Some(FilePosAnnot {
                value: $p,
                row,
                col,
            }) => {
                // These are required to make the linter shut up about
                // `unused variables`.
                let $row = row;
                let $col = col;
                $b
            }
            Some(FilePosAnnot { value: _, row, col }) => {
                return Statement::Malformed {
                    why: $err,
                    row,
                    col,
                }
            }
            None => {
                return Statement::Malformed {
                    why: PreprocError::UnexpectedEof {
                        hint: format!("I was looking for {}", $s),
                    },
                    row: 0,
                    col: 0,
                }
            }
        }
    }};
}

// collects items from the stream until `f` is true, and discards that item.
fn take_until<L, F>(stream: &mut L, f: F) -> Vec<TokenAnnot>
where
    L: Lexer,
    F: Fn(&Token) -> bool,
{
    let mut result = Vec::new();
    while let Some(t) = stream.next() {
        if f(t.borrow_value()) {
            break;
        }

        result.push(t);
    }

    result
}

macro_rules! expect_end_of_line {
    ( $stream:expr, $directive:expr ) => {{
        let tokens = take_until($stream, |t| matches!(t, Break));

        if let Some(FilePosAnnot { value: _, row, col }) = tokens.first() {
            return Statement::Malformed {
                why: PreprocError::ExpectBreak {
                    directive: $directive,
                },
                row: *row,
                col: *col,
            };
        }
    }};
}

fn expect_full_line<L>(
    result: Statement,
    directive: &'static str,
    stream: &mut L,
) -> Statement
where
    L: Lexer,
{
    expect_end_of_line!(stream, directive);
    result
}

fn consume_full_line<L>(result: Statement, stream: &mut L) -> Statement
where
    L: Lexer,
{
    take_until(stream, |t| matches!(t, Break));
    result
}

fn malformed_lex(e: FilePosAnnot<LexError>) -> Statement {
    let FilePosAnnot {
        value: err,
        row,
        col,
    } = e;

    Statement::Malformed {
        why: PreprocError::LexError(err),
        row,
        col,
    }
}

pub fn ast<L>(stream: &mut L) -> Ast
where
    L: Lexer,
{
    let mut result = Vec::new();

    while let Some(head) = stream.next() {
        match head.borrow_value() {
            Break | Semi => (),
            _ => result.push(statement(head, stream)),
        };
    }

    result
}

// Invariant: Should always consume at least one `Break` token if it does not
// reach the end of the file.
fn statement<L>(head: TokenAnnot, stream: &mut L) -> Statement
where
    L: Lexer,
{
    let TokenAnnot { value, row, col } = head;

    match value {
        Directive(d) => dispatch_directive(d, row, col, stream),
        _ => match tokens(stream) {
            Ok(mut t) => {
                t.insert(0, TokenAnnot { value, row, col });
                Statement::Tokens(t)
            }
            Err(e) => malformed_lex(e),
        },
    }
}

fn tokens<L>(stream: &mut L) -> Result<Vec<TokenAnnot>, FilePosAnnot<LexError>>
where
    L: Lexer,
{
    let mut buf = Vec::new();
    // I prefer to avoid structuring the loop like this, but we need it to
    // ensure that we always consume to the end of the line.
    let mut err = None;

    while let Some(t) = stream.next() {
        let FilePosAnnot { value, row, col } = t;

        match value {
            Break => break,
            Error(e) => {
                // This short-circuits on the first lexing error encountered
                // on a given line, instead of collecting them.
                err = Some(Err(FilePosAnnot { value: e, row, col }));
                break;
            }
            _ => buf.push(TokenAnnot {
                value: Token::from(value),
                row,
                col,
            }),
        }
    }

    match err {
        None => Ok(buf),
        Some(e) => e,
    }
}

fn dispatch_directive<L>(
    d: Directive,
    row: usize,
    col: usize,
    stream: &mut L,
) -> Statement
where
    L: Lexer,
{
    use token::Directive::*;

    match d {
        Include => expect_full_line(include(stream), "include", stream),
        Incbin => expect_full_line(incbin(stream), "incbin", stream),
        IfDef => ifdef(true, stream),
        IfNDef => ifdef(false, stream),
        Define => expect_full_line(define(stream), "define", stream),
        Else => consume_full_line(
            Statement::Malformed {
                why: PreprocError::StandaloneElse,
                row,
                col,
            },
            stream,
        ),
        Endif => consume_full_line(
            Statement::Malformed {
                why: PreprocError::StandaloneEndif,
                row,
                col,
            },
            stream,
        ),
        Undef => expect_full_line(undef(stream), "undef", stream),
        Pool => expect_full_line(Statement::Pool, "pool", stream),
        Incext => incext(row, col, stream),
        Inctevent => inctevent(row, col, stream),
    }
}

fn include<L>(stream: &mut L) -> Statement
where
    L: Lexer,
{
    let path = expect!(
        stream,
        Filepath(p),
        _,
        _,
        { p },
        PreprocError::ExpectIncludePath,
        "a filepath"
    );

    Statement::Include(path)
}

fn incbin<L>(stream: &mut L) -> Statement
where
    L: Lexer,
{
    let path = expect!(
        stream,
        Filepath(p),
        _,
        _,
        { p },
        PreprocError::ExpectIncbinPath,
        "a filepath"
    );

    Statement::Incbin(path)
}

enum DefBranch {
    Def,
    Ndef,
}

struct IfDefBuffer {
    which: DefBranch,
    def: Ast,
    ndef: Ast,
}

impl IfDefBuffer {
    fn new(which: DefBranch) -> Self {
        IfDefBuffer {
            which,
            def: Vec::new(),
            ndef: Vec::new(),
        }
    }

    fn push(&mut self, s: Statement) -> () {
        match self.which {
            DefBranch::Def => self.def.push(s),
            DefBranch::Ndef => self.ndef.push(s),
        }
    }

    fn flip(&mut self) -> () {
        match self.which {
            DefBranch::Def => self.which = DefBranch::Ndef,
            DefBranch::Ndef => self.which = DefBranch::Def,
        }
    }

    fn to_statement(self, id: String) -> Statement {
        let IfDefBuffer {
            which: _,
            def,
            ndef,
        } = self;

        Statement::IfDef(id, def, ndef)
    }
}

fn ifdef<L>(is_def: bool, stream: &mut L) -> Statement
where
    L: Lexer,
{
    use token::Directive::*;

    let (s, row, col) = expect!(
        stream,
        Ident(s),
        row,
        col,
        { (s, row, col) },
        PreprocError::ExpectIfdefName,
        "an identifier"
    );

    let () = expect!(
        stream,
        Break,
        _,
        _,
        { () },
        PreprocError::ExpectBreak { directive: "ifdef" },
        "a newline"
    );

    let mut buf = IfDefBuffer::new(if is_def {
        DefBranch::Def
    } else {
        DefBranch::Ndef
    });

    while let Some(t) = stream.next() {
        match t.borrow_value() {
            Directive(Endif) => {
                return expect_full_line(buf.to_statement(s), "endif", stream)
            }
            Directive(Else) => {
                expect!(
                    stream,
                    Break,
                    _,
                    _,
                    { () },
                    PreprocError::ExpectBreak { directive: "else" },
                    "a newline"
                );
                buf.flip();
            }
            _ => {
                let stmt = statement(t, stream);
                buf.push(stmt);
            }
        }
    }

    Statement::Malformed {
        why: PreprocError::UnclosedIf,
        row,
        col,
    }
}

fn unquote_definition(
    name: &String,
    body: String,
    fname: &String,
) -> Result<Vec<Token>, FilePosAnnot<LexError>> {
    let loc = format!(
        "definition of {name} (in {fname})",
        name = name,
        fname = fname
    );

    let mut stream = lex(loc, body.chars());

    tokens(&mut stream)
        .map(|ts| ts.into_iter().map(FilePosAnnot::extract_value).collect())
}

fn parse_args<L>(
    name: String,
    row: usize,
    col: usize,
    stream: &mut L,
) -> Result<IndexSet<String>, Statement>
where
    L: Lexer,
{
    let mut args = IndexSet::new();

    while let Some(t) = stream.next() {
        let FilePosAnnot { value, row, col } = t;

        match value {
            RParen => return Ok(args),
            Ident(s) => {
                // `IndexSet::insert` returns true if the item is not already
                // present.
                if !args.insert(s) {
                    return Err(Statement::Malformed {
                        why: PreprocError::DuplicateMacroArg { name },
                        row,
                        col,
                    });
                }
            }
            _ => {
                return Err(Statement::Malformed {
                    why: PreprocError::InvalidMacroArg,
                    row,
                    col,
                })
            }
        }
    }

    Err(Statement::Malformed {
        why: PreprocError::UnexpectedEof {
            hint: "is there an unclosed left paren?".to_string(),
        },
        row,
        col,
    })
}

fn definition_body<L>(name: &String, stream: &mut L) -> Result<Vec<Token>, Statement>
where
    L: Lexer,
{
    match stream.next() {
        Some(FilePosAnnot {
            value: Break,
            row,
            col,
        }) => Err(Statement::Malformed {
            why: PreprocError::EmptyMacroBody,
            row,
            col,
        }),
        Some(FilePosAnnot {
            value: QuotedString(body),
            row: _,
            col: _,
        }) => match unquote_definition(name, body, stream.filename()) {
            Ok(ts) => Ok(ts),
            Err(e) => Err(malformed_lex(e)),
        },
        Some(FilePosAnnot { value, row, col }) => match value {
            Error(e) => Err(malformed_lex(FilePosAnnot { value: e, row, col })),
            _ => Ok(vec![Token::from(value)]),
        },
        None => Err(Statement::Malformed {
            why: PreprocError::UnexpectedEof {
                hint: "is there an unclosed left paren?".to_string(),
            },
            row: 0,
            col: 0,
        }),
    }
}

fn define<L>(stream: &mut L) -> Statement
where
    L: Lexer,
{
    let s = expect!(
        stream,
        Ident(s),
        _,
        _,
        { s },
        PreprocError::ExpectDefineName,
        "an identifier"
    );

    match stream.next() {
        Some(FilePosAnnot {
            value: Break,
            row: _,
            col: _,
        })
        | None => Statement::Define(s, Definition::Empty),
        Some(FilePosAnnot {
            value: LParen,
            row,
            col,
        }) => match parse_args(s.clone(), row, col, stream) {
            Err(e) => e,
            Ok(args) => match definition_body(&s, stream) {
                Ok(ts) => {
                    Statement::Define(s, Definition::Macro(args, VecDeque::from(ts)))
                }
                Err(e) => e,
            },
        },
        Some(t) => match definition_body(&s, stream) {
            Ok(ts) => {
                let mut ts = VecDeque::from(ts);
                ts.push_front(Token::from(t.extract_value()));
                Statement::Define(s, Definition::Rename(ts))
            }
            Err(e) => e,
        },
    }
}

fn undef<L>(stream: &mut L) -> Statement
where
    L: Lexer,
{
    let s = expect!(
        stream,
        Ident(s),
        _,
        _,
        { s },
        PreprocError::ExpectUndefName,
        "an identifier"
    );

    Statement::Undef(s)
}

fn parse_filepath(s: String) -> RelativePathBuf {
    RelativePathBuf::from(s.replace("\\", "/"))
}

fn incext<L>(row: usize, col: usize, stream: &mut L) -> Statement
where
    L: Lexer,
{
    // XXX - currently, there is no way for this to ever accept a `Filepath`.
    // An invocation like `#incext Foo.exe` would be `Ident("Foo"), Dot,
    // Ident("exe")`, which we'd need to reconstruct.
    let prog = match stream.next() {
        Some(FilePosAnnot {
            value: QuotedString(s),
            row: _,
            col: _,
        })
        | Some(FilePosAnnot {
            value: Ident(s),
            row: _,
            col: _,
        }) => parse_filepath(s),
        Some(FilePosAnnot {
            value: Filepath(prog),
            row: _,
            col: _,
        }) => prog,
        Some(FilePosAnnot { value: _, row, col }) => {
            return Statement::Malformed {
                why: PreprocError::ExtBadProgram {
                    directive: "incext",
                },
                row,
                col,
            }
        }
        None => {
            return Statement::Malformed {
                why: PreprocError::UnexpectedEof {
                    hint: "#incext should be followed by a program name".to_string(),
                },
                row,
                col,
            }
        }
    };

    let rest = take_until(stream, |t| matches!(t, Break))
        .into_iter()
        .map(
            |FilePosAnnot {
                 value,
                 row: _,
                 col: _,
             }| value,
        )
        .collect();

    Statement::Incext(prog, rest)
}

fn inctevent<L>(row: usize, col: usize, stream: &mut L) -> Statement
where
    L: Lexer,
{
    // XXX - currently, there is no way for this to ever accept a `Filepath`.
    // An invocation like `#incext Foo.exe` would be `Ident("Foo"), Dot,
    // Ident("exe")`, which we'd need to reconstruct.
    let prog = match stream.next() {
        Some(FilePosAnnot {
            value: QuotedString(s),
            row: _,
            col: _,
        })
        | Some(FilePosAnnot {
            value: Ident(s),
            row: _,
            col: _,
        }) => parse_filepath(s),
        Some(FilePosAnnot {
            value: Filepath(prog),
            row: _,
            col: _,
        }) => prog,
        Some(FilePosAnnot { value: _, row, col }) => {
            return Statement::Malformed {
                why: PreprocError::ExtBadProgram {
                    directive: "inctevent",
                },
                row,
                col,
            }
        }
        None => {
            return Statement::Malformed {
                why: PreprocError::UnexpectedEof {
                    hint: "#inctevent should be followed by a program name".to_string(),
                },
                row,
                col,
            }
        }
    };

    let rest = take_until(stream, |t| matches!(t, Break))
        .into_iter()
        .map(
            |FilePosAnnot {
                 value,
                 row: _,
                 col: _,
             }| value,
        )
        .collect();

    Statement::Inctevent(prog, rest)
}
