use std::collections::VecDeque;

use indexmap::set::IndexSet;
use relative_path::RelativePathBuf;

use super::{
    super::lex::{lex, Lexer, TokenAnnot as LexTokenAnnot},
    syntax::{convert_token, Ast, Definition, Statement, Token, TokenAnnot},
    token,
    token::{Directive, FilePosAnnot},
    PreprocError,
};
use crate::writer::{Logger, LoggerContext};

use token::Token::*;

#[cfg(test)]
mod tests;

pub type Error = FilePosAnnot<PreprocError>;
pub type Output<T> = Logger<T, Error>;
type ErrorLog = LoggerContext<Error>;

fn log_error(log: &mut ErrorLog, err: PreprocError, row: usize, col: usize) -> () {
    log.log(FilePosAnnot {
        value: err,
        row,
        col,
    });
}

// collects items from the stream until `f` is true, and discards that item.
fn take_until<L, F>(f: F, stream: &mut L, errs: &mut ErrorLog) -> Vec<TokenAnnot>
where
    L: Lexer,
    F: Fn(&Token) -> bool,
{
    let mut result = Vec::new();

    while let Some(t) = stream.next() {
        match t {
            FilePosAnnot {
                value: Error(e),
                row,
                col,
            } => log_error(errs, PreprocError::LexError(e), row, col),
            FilePosAnnot { value, row, col } => {
                let value = convert_token(value);
                if f(&value) {
                    break;
                }
                result.push(FilePosAnnot { value, row, col });
            }
        }
    }

    result
}

fn expect_end_of_line<L>(
    directive: &'static str,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> ()
where
    L: Lexer,
{
    let ts = take_until(|t| matches!(t, Break), stream, errs);

    for t in ts.into_iter() {
        errs.log(t.map(|_| PreprocError::ExpectBreak { directive }));
        break;
    }
}

macro_rules! expect {
    ( $p:pat, $body:block, $error:expr, $wanted:expr, $stream:expr, $errs:expr ) => {
        match $stream.next() {
            None => {
                log_error(
                    $errs,
                    PreprocError::UnexpectedEof {
                        hint: format!("I was looking for {}", $wanted),
                    },
                    0,
                    0,
                );
                return Statement::Malformed;
            }
            Some(FilePosAnnot {
                value: $p,
                row,
                col,
            }) => FilePosAnnot {
                value: $body,
                row,
                col,
            },
            Some(FilePosAnnot { value: _, row, col }) => {
                log_error($errs, $error, row, col);
                return Statement::Malformed;
            }
        }
    };
}

fn expect_full_line<T, L>(
    result: T,
    directive: &'static str,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> T
where
    L: Lexer,
{
    expect_end_of_line(directive, stream, errs);
    result
}

pub fn ast<L>(stream: &mut L) -> Output<Ast>
where
    L: Lexer,
{
    let mut result = Output::pure(Vec::new());

    while let Some(FilePosAnnot { value, row, col }) = stream.next() {
        match value {
            Break | Semi => (),
            Error(e) => result.log(FilePosAnnot {
                value: PreprocError::LexError(e),
                row,
                col,
            }),
            _ => result.and_then_mut(|ast, errs| {
                let stmt = statement(
                    FilePosAnnot {
                        value: convert_token(value),
                        row,
                        col,
                    },
                    stream,
                    errs,
                );
                ast.push(stmt)
            }),
        };
    }

    result
}

// Invariant: Should always consume at least one `Break` token if it does not
// reach the end of the file.
fn statement<L>(head: TokenAnnot, stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    match head.borrow_value() {
        Directive(d) => dispatch_directive(*d, head.row, head.col, stream, errs),
        _ => Statement::Tokens(tokens(head, stream, errs)),
    }
}

fn tokens<L>(head: TokenAnnot, stream: &mut L, errs: &mut ErrorLog) -> Vec<TokenAnnot>
where
    L: Lexer,
{
    let mut buf = Vec::new();
    buf.push(head);

    while let Some(FilePosAnnot { value, row, col }) = stream.next() {
        match value {
            Break => break,
            Error(e) => errs.log(FilePosAnnot {
                value: PreprocError::LexError(e),
                row,
                col,
            }),
            _ => buf.push(FilePosAnnot {
                value: convert_token(value),
                row,
                col,
            }),
        }
    }

    buf
}

fn dispatch_directive<L>(
    d: Directive,
    row: usize,
    col: usize,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> Statement
where
    L: Lexer,
{
    use token::Directive::*;

    match d {
        Include => expect_full_line(include(stream, errs), "include", stream, errs),
        Incbin => expect_full_line(incbin(stream, errs), "incbin", stream, errs),
        IfDef => ifdef(true, stream, errs),
        IfNDef => ifdef(false, stream, errs),
        Define => expect_full_line(define(stream, errs), "define", stream, errs),
        Else => {
            log_error(errs, PreprocError::StandaloneElse, row, col);
            let _ = take_until(|t| matches!(t, Break), stream, errs);
            Statement::Malformed
        }
        Endif => {
            log_error(errs, PreprocError::StandaloneEndif, row, col);
            let _ = take_until(|t| matches!(t, Break), stream, errs);
            Statement::Malformed
        }
        Undef => expect_full_line(undef(stream, errs), "undef", stream, errs),
        Pool => {
            expect_end_of_line("pool", stream, errs);
            Statement::Pool
        }
        Incext => incext(row, col, stream, errs),
        Inctevent => inctevent(row, col, stream, errs),
    }
}

fn include<L>(stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    let path = expect!(
        Filepath(p),
        { p },
        PreprocError::ExpectIncludePath,
        "a filepath",
        stream,
        errs
    )
    .extract_value();

    Statement::Include(path)
}

fn incbin<L>(stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    let path = expect!(
        Filepath(p),
        { p },
        PreprocError::ExpectIncbinPath,
        "a filepath",
        stream,
        errs
    )
    .extract_value();

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

fn ifdef<L>(is_def: bool, stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    use token::Directive::*;

    let FilePosAnnot { value: s, row, col } = expect!(
        Ident(s),
        { s },
        PreprocError::ExpectIfdefName,
        "an identifier",
        stream,
        errs
    );

    expect_end_of_line("ifdef", stream, errs);

    let mut buf = IfDefBuffer::new(if is_def {
        DefBranch::Def
    } else {
        DefBranch::Ndef
    });

    while let Some(FilePosAnnot { value, row, col }) = stream.next() {
        match value {
            Directive(Endif) => {
                return expect_full_line(buf.to_statement(s), "endif", stream, errs)
            }
            Directive(Else) => {
                expect_end_of_line("else", stream, errs);
                buf.flip();
            }
            Error(e) => errs.log(FilePosAnnot {
                value: PreprocError::LexError(e),
                row,
                col,
            }),
            _ => {
                let stmt = statement(
                    FilePosAnnot {
                        value: convert_token(value),
                        row,
                        col,
                    },
                    stream,
                    errs,
                );
                buf.push(stmt);
            }
        }
    }

    log_error(errs, PreprocError::UnclosedIf, row, col);

    Statement::Malformed
}

fn unquote_definition(
    name: &String,
    body: String,
    fname: &String,
) -> Option<Output<Vec<Token>>> {
    let loc = format!(
        "definition of {name} (in {fname})",
        name = name,
        fname = fname
    );

    let mut stream = lex(loc, body.chars());
    let mut errs = ErrorLog::new(Vec::new());

    // XXX - this is kind of gross
    while let Some(FilePosAnnot { value, row, col }) = stream.next() {
        match value {
            Error(e) => errs.log(FilePosAnnot {
                value: PreprocError::LexError(e),
                row,
                col,
            }),
            _ => {
                let result = tokens(
                    FilePosAnnot {
                        value: convert_token(value),
                        row,
                        col,
                    },
                    &mut stream,
                    &mut errs,
                );
                return Some(Output::with_context(
                    result
                        .into_iter()
                        .map(FilePosAnnot::extract_value)
                        .collect(),
                    errs.map(|errs| {
                        errs.into_iter()
                            .map(|err| match err {
                                FilePosAnnot {
                                    value: PreprocError::LexError(e),
                                    row,
                                    col,
                                } => FilePosAnnot {
                                    value: PreprocError::LexErrorInMacroBody {
                                        err: e,
                                        name: name.clone(),
                                        fname: fname.clone(),
                                    },
                                    row,
                                    col,
                                },
                                _ => panic!(concat!(
                                    "BUG (unquote_definition): ",
                                    "`lang::preprocess::parse::tokens` ",
                                    "logged error other than `LexError`"
                                )),
                            })
                            .collect()
                    }),
                ));
            }
        }
    }

    None
}

fn parse_args<L>(
    name: String,
    row: usize,
    col: usize,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> Option<IndexSet<String>>
where
    L: Lexer,
{
    let mut args = IndexSet::new();

    while let Some(t) = stream.next() {
        let FilePosAnnot { value, row, col } = t;

        match value {
            RParen => return Some(args),
            Ident(s) => {
                // `IndexSet::insert` returns true if the item is not already
                // present.
                if !args.insert(s) {
                    log_error(
                        errs,
                        PreprocError::DuplicateMacroArg { name: name.clone() },
                        row,
                        col,
                    )
                }
            }
            _ => log_error(errs, PreprocError::InvalidMacroArg, row, col),
        }
    }

    errs.log(FilePosAnnot {
        value: PreprocError::UnexpectedEof {
            hint: "is there an unclosed left paren?".to_string(),
        },
        row,
        col,
    });

    None
}

fn definition_body<L>(
    name: &String,
    body: Option<LexTokenAnnot>,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> Vec<Token>
where
    L: Lexer,
{
    match body {
        // This case is only reachable via `#define foo(a,b) [BREAK]`.
        Some(FilePosAnnot {
            value: Break,
            row,
            col,
        }) => {
            log_error(errs, PreprocError::EmptyMacroBody, row, col);
            Vec::new()
        }
        Some(FilePosAnnot {
            value: QuotedString(body),
            row,
            col,
        }) => match unquote_definition(name, body, stream.filename()) {
            Some(w) => {
                let (ts, macro_errs) = w.extract();
                errs.log_many(macro_errs);
                ts
            }
            None => {
                log_error(errs, PreprocError::EmptyMacroBody, row, col);
                Vec::new()
            }
        },
        Some(FilePosAnnot { value, row, col }) => match value {
            Error(e) => {
                log_error(errs, PreprocError::LexError(e), row, col);
                Vec::new()
            }
            _ => vec![convert_token(value)],
        },
        None => {
            errs.log(FilePosAnnot {
                value: PreprocError::UnexpectedEof {
                    hint: "is there an unclosed left paren?".to_string(),
                },
                row: 0,
                col: 0,
            });
            Vec::new()
        }
    }
}

fn define<L>(stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    let FilePosAnnot {
        value: s,
        row: _,
        col: _,
    } = expect!(
        Ident(s),
        { s },
        PreprocError::ExpectDefineName,
        "an identifier",
        stream,
        errs
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
        }) => match parse_args(s.clone(), row, col, stream, errs) {
            None => Statement::Malformed,
            Some(args) => {
                let ts = definition_body(&s, stream.next(), stream, errs);
                Statement::Define(s, Definition::Macro(args, VecDeque::from(ts)))
            }
        },
        t @ Some(_) => {
            let ts = VecDeque::from(definition_body(&s, t, stream, errs));
            Statement::Define(s, Definition::Rename(ts))
        }
    }
}

fn undef<L>(stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
    let FilePosAnnot {
        value: s,
        row: _,
        col: _,
    } = expect!(
        Ident(s),
        { s },
        PreprocError::ExpectUndefName,
        "an identifier",
        stream,
        errs
    );

    Statement::Undef(s)
}

fn parse_filepath(s: String) -> RelativePathBuf {
    RelativePathBuf::from(s.replace("\\", "/"))
}

fn incext<L>(row: usize, col: usize, stream: &mut L, errs: &mut ErrorLog) -> Statement
where
    L: Lexer,
{
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
        // XXX - currently, this case is impossible to actually reach. An
        // invocation like `#incext Foo.exe` would be `Ident("Foo"), Dot,
        // Ident("exe")`, which we'd need to reconstruct, so we'll mandate
        // using `"Foo.exe"` instead.
        Some(FilePosAnnot {
            value: Filepath(prog),
            row: _,
            col: _,
        }) => prog,
        Some(FilePosAnnot { value: _, row, col }) => {
            errs.log(FilePosAnnot {
                value: PreprocError::ExtBadProgram {
                    directive: "incext",
                },
                row,
                col,
            });
            return Statement::Malformed;
        }
        None => {
            errs.log(FilePosAnnot {
                value: PreprocError::UnexpectedEof {
                    hint: "#incext should be followed by a program name".to_string(),
                },
                row,
                col,
            });
            return Statement::Malformed;
        }
    };

    let rest = take_until(|t| matches!(t, Break), stream, errs)
        .into_iter()
        .map(FilePosAnnot::extract_value)
        .collect();

    Statement::Incext(prog, rest)
}

fn inctevent<L>(
    row: usize,
    col: usize,
    stream: &mut L,
    errs: &mut ErrorLog,
) -> Statement
where
    L: Lexer,
{
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
            errs.log(FilePosAnnot {
                value: PreprocError::ExtBadProgram {
                    directive: "inctevent",
                },
                row,
                col,
            });
            return Statement::Malformed;
        }
        None => {
            errs.log(FilePosAnnot {
                value: PreprocError::UnexpectedEof {
                    hint: "#inctevent should be followed by a program name".to_string(),
                },
                row,
                col,
            });
            return Statement::Malformed;
        }
    };

    let rest = take_until(|t| matches!(t, Break), stream, errs)
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
