
use std::collections::VecDeque;

use error_chain::bail;

use super::lex::{Lexable};
use super::lex::errors::{Result};

pub enum Token
{ Ident(String)
, Number(i32)
, Colon
, Dash, Slash
, Star, Plus
, Hash
, LParen, RParen
, LBrack, RBrack
, Break
}

enum Radix
{ Dec
, Hex
}

impl Radix {
    pub fn base(&self) -> u32 {
        match self
        { Radix::Dec => 10
        , Radix::Hex => 16
        }
    }
}

enum CommentType { Line, Block }

enum Mode
{ Wait
, Ident
, Num(Radix)
, OnZero
, OnSlash
, OnStar
}

pub struct LexBuf
{ buf: VecDeque<char>
, mode: Mode
, comment_stack: Vec<CommentType>
, output_queue: VecDeque<Box<Token>>
}

impl LexBuf {
    fn new() -> Self {
        LexBuf
        { buf: VecDeque::new()
        , mode: Mode::Wait
        , comment_stack: Vec::new()
        , output_queue: VecDeque::new()
        }
    }

    fn set_mode_only(&mut self, m: Mode) -> Result<()> {
        self.mode = m;
        Ok(())
    }

    fn exit_comment(&mut self) -> Result<()> {
        if let Some(_) = self.comment_stack.pop() {
            self.set_mode_only(Mode::Wait)
        }
        // This should never happen
        else {
            bail!("internal error: attempted to exit_comment with empty stack")
        }
    }

    fn enter_comment(&mut self, ty: CommentType) -> Result<()> {
        self.comment_stack.push(ty);
        Ok(())
    }

    fn emit(&mut self, tok: Token) -> Result<()> {
        self.buf.clear();
        self.output_queue.push_back(Box::new(tok));
        Ok(())
    }

    fn fail(&self, msg: String) -> Result<()> {
        bail!(msg)
    }

    fn handle_block_comment(&mut self, c: char) -> Result<()> {
        match &self.mode {
            Mode::OnStar => {
                match c
                { '/' => self.exit_comment()
                , '*' => self.set_mode_only(Mode::OnStar)
                , _ => self.set_mode_only(Mode::Wait)
                }
            },
            Mode::OnSlash => {
                match c
                { '/' => self.enter_comment(CommentType::Line)
                , '*' => self.enter_comment(CommentType::Block)
                , _ => Ok(())
                }
            },
            _ => {
                match c
                { '/' => self.set_mode_only(Mode::OnSlash)
                , '*' => self.set_mode_only(Mode::OnStar)
                , _ => Ok(())
                }
            }
        }
    }

    fn push(&mut self, c: char) -> Result<()> {
        self.buf.push_back(c);
        Ok(())
    }

    fn handle_char(&mut self, c: char) -> Result<()> {
        if c.is_whitespace() { Ok(()) }
        else {
            match c
            { '-' => self.emit(Token::Dash)
            , ':' => self.emit(Token::Colon)
            , ';' => self.emit(Token::Break)
            , '+' => self.emit(Token::Plus)
            , '#' => self.emit(Token::Hash)
            , '(' => self.emit(Token::LParen)
            , ')' => self.emit(Token::RParen)
            , '[' => self.emit(Token::LBrack)
            , ']' => self.emit(Token::RBrack)
            , '0' => self.set_mode_only(Mode::OnZero)
            , '/' => self.set_mode_only(Mode::OnSlash)
            , '*' => self.set_mode_only(Mode::OnStar)
            , 'a'..='z' | 'A'..='Z' | '_' => self.set_mode_only(Mode::Ident)
            , _ => self.fail(format!("unrecognized character: {}", c))
            }
        }
    }

    fn dispatch(&mut self, c: char) -> Result<()> {
        match &self.mode {
            Mode::Ident => {
                if c.is_alphanumeric() || c == '_' { self.push(c) }
                else {
                    self.mode = Mode::Wait;
                    let s: String = self.buf.iter().collect();
                    self.emit(Token::Ident(s))
                }
            },
            Mode::Num(r) => {
                let r = r.base();
                if c.is_digit(r) { self.push(c) }
                else if c == 'b' || c == 'B' {
                    let s: String = self.buf.iter().collect();
                    match i32::from_str_radix(&s, 2) {
                        Err(e) => self.fail(e.to_string()),
                        Ok(i) => self.emit(Token::Number(i))
                    }
                }
                else if c.is_whitespace() {
                    let s: String = self.buf.iter().collect();
                    match i32::from_str_radix(&s, r) {
                        Err(e) => self.fail(e.to_string()),
                        Ok(i) => self.emit(Token::Number(i))
                    }
                }
                else {
                    self.fail(format!(
                            "bad character in base {} numeric literal: {}",
                            r, c))
                }
            },
            Mode::OnZero => {
                if c == 'x' {
                    self.set_mode_only(Mode::Num(Radix::Hex))
                }
                else if c.is_digit(10) {
                    self.mode = Mode::Num(Radix::Dec);
                    self.push(c)
                }
                else {
                    self.fail(format!(
                            "bad character in base 10 numeric literal: {}", c))
                }
            },
            Mode::OnSlash => {
                match c
                { '*' => self.enter_comment(CommentType::Block)
                , '/' => self.enter_comment(CommentType::Line)
                , _ => self.emit(Token::Slash)
                }
            },
            Mode::OnStar => {
                match c
                { '/' => self.fail("orphaned end-comment marker".to_string())
                , _ => self.emit(Token::Star)
                }
            },
            Mode::Wait => self.handle_char(c)
        }?;

        if c == '\n' { self.emit(Token::Break) }
        else { Ok(()) }
    }

    fn handle(&mut self, c: char) -> Result<()> {
        match self.comment_stack.first() {
            Some(CommentType::Line) => {
                if c == '\n' {
                    let _ = self.exit_comment();
                    Ok(())
                }
                else { Ok(()) }
            },
            Some(CommentType::Block) => self.handle_block_comment(c),
            None => self.dispatch(c)
        }
    }
}

impl Lexable for Token {
    type LexBuf = LexBuf;

    fn newbuf() -> LexBuf { LexBuf::new() }

    fn push(lexbuf: &mut LexBuf, c: char) -> Result<()> {
        lexbuf.handle(c)?;
        Ok(())
    }

    fn next(lexbuf: &mut LexBuf) -> Option<Box<Self>> {
        lexbuf.output_queue.pop_back()
    }
}

