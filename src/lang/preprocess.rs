use std::collections::VecDeque;

use thiserror::Error;

use genawaiter::rc::{Co, Gen};

use directive::DirectiveError as DError;

pub mod directive;
pub mod state;

use state::State;

#[derive(Error, Debug)]
pub enum PreprocError {
    #[error("found bad character {chr:?}")]
    LexError { chr: char },
    #[error(transparent)]
    DirectiveError(#[from] DError),
    #[error("unclosed quoted string")]
    UnclosedQuote,
    #[error("unclosed block comment")]
    UnclosedComment,
    #[error("invalid escape sequence")]
    Unescape(#[from] snailquote::UnescapeError),
    #[error("bad number for base {base:?}: {s:?}")]
    InvalidBaseNum { s: String, base: usize },
    #[error("number {n:?} out of range for 32 bits")]
    Overflow { n: u64 },
    #[error("found bad character #; directives go on their own line!")]
    BadHash,
}

#[derive(Debug)]
pub enum Token {
    Ident(String),
    Number(u32),
    QuotedString(String),
    Colon,
    Dash,
    Slash,
    Star,
    Plus,
    Percent,
    Ampersand,
    Bar,
    Caret,
    LShift,
    RShift,
    //Hash,
    Comma,
    LCurly,
    RCurly,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LAngle,
    RAngle,
    Break,
    Semi,
    Error(PreprocError),
}

#[derive(Debug)]
pub struct FilePosAnnot<T> {
    value: T,
    fname: String,
    row: usize,
    col: usize,
}

// XXX: need a better name for this
struct FilePosStream<I: Iterator> {
    wrapped: I,
    fname: String,
    row: usize,
    col: usize,
    // XXX: this is probably overkill
    buffer: VecDeque<FilePosAnnot<I::Item>>,
    comment_nesting: usize,
    in_line_comment: bool,
}

// Due to how Rust handles coroutines, recursively calling `preprocess` is
// unwieldy and difficult. Instead, we keep a stack of streams, each corresponding
// to the current file we are in.
pub struct StreamStack(Vec<FilePosStream<Box<dyn Iterator<Item = char>>>>);

pub type TokenAnnot = FilePosAnnot<Token>;

enum DispatchResult {
    DoNothing,
    StartOfLine,
}

impl<I> FilePosStream<I>
where
    I: Iterator,
{
    fn annot(&self, value: I::Item) -> FilePosAnnot<I::Item> {
        FilePosAnnot {
            value,
            fname: self.fname.clone(),
            row: self.row,
            col: self.col,
        }
    }

    fn buffer(&mut self, value: I::Item) {
        self.buffer.push_back(self.annot(value));
    }

    fn advance_row(&mut self) {
        self.row += 1;
        self.col = 1;
    }

    fn advance_col(&mut self) {
        self.col += 1;
    }
}

impl<I> FilePosStream<I>
where
    I: Iterator<Item = char> + 'static,
{
    fn new(fname: String, stream: I) -> FilePosStream<Box<dyn Iterator<Item = char>>> {
        FilePosStream {
            wrapped: Box::new(stream),
            fname,
            row: 1,
            col: 1,
            buffer: VecDeque::new(),
            comment_nesting: 0,
            in_line_comment: false,
        }
    }

    // XXX: This is basically stacking two iterators on top of each other.
    fn advance(&mut self) -> Option<FilePosAnnot<char>> {
        if let Some(result) = self.buffer.pop_front() {
            return Some(result);
        }

        self.wrapped.next().and_then(|c| match c {
            '\\' => match self.wrapped.next() {
                Some('\n') => {
                    self.advance_row();
                    self.wrapped.next().map(|c| self.annot(c))
                }
                Some(c) => {
                    let result = self.annot('\\');
                    self.advance_col();
                    self.buffer(c);

                    Some(result)
                }
                None => Some(self.annot('\\')),
            },
            '\n' => {
                let result = self.annot('\n');
                self.advance_row();
                Some(result)
            }
            _ => {
                let result = self.annot(c);
                self.advance_col();
                self.in_line_comment = false;
                Some(result)
            }
        })
    }
}

// This entire song and dance is effectively to strip escaped newlines from
// the input stream without compromising the file position.
impl<I> Iterator for FilePosStream<I>
where
    I: Iterator<Item = char> + 'static,
{
    type Item = FilePosAnnot<char>;

    fn next(&mut self) -> Option<Self::Item> {
        // XXX: This is just structured really weirdly
        loop {
            while self.in_line_comment {
                if let '\n' = self.advance()?.value {
                    self.in_line_comment = false;
                }
            }

            while self.comment_nesting > 0 {
                match self.advance()?.value {
                    '*' => match self.advance()?.value {
                        '/' => self.comment_nesting -= 1,
                        _ => (),
                    },
                    '/' => match self.advance()?.value {
                        '/' => self.in_line_comment = true,
                        '*' => self.comment_nesting += 1,
                        _ => (),
                    },
                    _ => (),
                }
            }

            let c = self.advance()?;

            match c.value {
                '/' => match self.advance()?.value {
                    '/' => self.in_line_comment = true,
                    '*' => self.comment_nesting += 1,
                    c2 => {
                        self.buffer(c2);
                        return Some(c);
                    }
                },
                _ => return Some(c),
            }
        }
    }
}

impl Iterator for StreamStack {
    type Item = FilePosAnnot<char>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.last_mut() {
            None => return None,
            Some(s) => match s.next() {
                Some(r) => return Some(r),
                None => (),
            },
        };

        let s = self.0.pop().unwrap();
        // XXX - this is a hack, and is basically the express purpose of sum
        // types. however, adjusting this to properly output a `file break'
        // token would require threading this sum through all of the other
        // functions, and that's just annoying.
        //
        // In theory, it's possible for some #inctevent or #include to contain
        // these characters, but we can just tell them not to do that.
        if s.comment_nesting == 0 {
            return Some(FilePosAnnot {
                value: '\x01',
                fname: "intermediate filestream".to_string(),
                col: 0,
                row: 0,
            });
        } else {
            return Some(FilePosAnnot {
                value: '\x02',
                fname: "intermediate filestream".to_string(),
                col: 0,
                row: 0,
            });
        }
    }
}

impl StreamStack {
    fn new(v: FilePosStream<Box<dyn Iterator<Item = char>>>) -> Self {
        let mut result = Vec::new();
        result.push(v);
        StreamStack(result)
    }

    fn buffer(&mut self, c: char) -> () {
        self.0.last_mut().unwrap().buffer(c)
    }

    fn push(&mut self, v: FilePosStream<Box<dyn Iterator<Item = char>>>) {
        self.0.push(v);
    }

    fn curr_fname(&self) -> &String {
        let StreamStack(v) = self;
        &v.last().unwrap().fname
    }

    fn curr_row(&self) -> usize {
        let StreamStack(v) = self;
        v.last().unwrap().row
    }

    fn next_char_only(&mut self) -> Option<char> {
        self.next().map(
            |FilePosAnnot {
                 value,
                 row: _,
                 col: _,
                 fname: _,
             }| value,
        )
    }

    fn next_non_ws(&mut self) -> Option<char> {
        self.skip_while(|c| c.is_whitespace() && c != '\n');
        self.next_char_only()
    }

    fn to_end_of_line(&mut self) -> String {
        let mut result = String::new();
        let curr_row = self.curr_row();

        while let Some(FilePosAnnot {
            value,
            row,
            col: _,
            fname: _,
        }) = self.next()
        {
            if value == '\n' || row != curr_row {
                self.buffer(value);
                break;
            }

            result.push(value);
        }

        result
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut result = String::new();

        while let Some(c) = self.next_char_only() {
            if !pred(c) {
                self.buffer(c);
                break;
            }

            result.push(c);
        }

        result
    }

    fn skip_while<F: Fn(char) -> bool>(&mut self, pred: F) -> () {
        self.take_while(pred);
    }

    async fn yield_single(
        &self,
        co: &Co<TokenAnnot, ()>,
        value: Token,
        row: usize,
        col: usize,
    ) -> () {
        co.yield_(TokenAnnot {
            value,
            row,
            col,
            fname: self.curr_fname().clone(),
        })
        .await;
    }

    async fn dispatch(
        &mut self,
        co: &Co<TokenAnnot, ()>,
        c: char,
        row: usize,
        col: usize,
    ) -> DispatchResult {
        use DispatchResult::*;
        use Token::*;

        match c {
            ';' => self.yield_single(co, Semi, row, col).await,
            ':' => self.yield_single(co, Colon, row, col).await,
            '{' => self.yield_single(co, LCurly, row, col).await,
            '}' => self.yield_single(co, RCurly, row, col).await,
            '[' => self.yield_single(co, LBrack, row, col).await,
            ']' => self.yield_single(co, RBrack, row, col).await,
            '(' => self.yield_single(co, LParen, row, col).await,
            ')' => self.yield_single(co, RParen, row, col).await,
            '*' => self.yield_single(co, Star, row, col).await,
            '%' => self.yield_single(co, Percent, row, col).await,
            ',' => self.yield_single(co, Comma, row, col).await,
            // comments are handled below
            '/' => self.yield_single(co, Slash, row, col).await,
            '+' => self.yield_single(co, Plus, row, col).await,
            '-' => self.yield_single(co, Dash, row, col).await,
            '&' => self.yield_single(co, Ampersand, row, col).await,
            '^' => self.yield_single(co, Caret, row, col).await,
            '|' => self.yield_single(co, Bar, row, col).await,
            '"' => {
                let result = self.quoted_string();
                self.yield_single(co, result, row, col).await
            }
            '<' => {
                if let Some('<') = self.next_char_only() {
                    self.yield_single(co, LShift, row, col).await;
                } else {
                    self.buffer(c);
                    self.yield_single(co, LAngle, row, col).await;
                }
            }
            '>' => {
                if let Some('>') = self.next_char_only() {
                    self.yield_single(co, RShift, row, col).await;
                } else {
                    self.buffer(c);
                    self.yield_single(co, RAngle, row, col).await;
                }
            }
            '\n' => {
                self.yield_single(co, Break, row, col).await;
                return StartOfLine;
            }
            '#' => {
                self.yield_single(co, Error(PreprocError::BadHash), row, col)
                    .await
            }
            '$' => {
                let result = self.hexadecimal();
                self.yield_single(co, result, row, col).await
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let result = self.ident(c);
                self.yield_single(co, result, row, col).await
            }
            '0' => match self.next_char_only() {
                Some('x') => {
                    let result = self.hexadecimal();
                    self.yield_single(co, result, row, col).await
                }
                Some('b') => {
                    let result = self.binary();
                    self.yield_single(co, result, row, col).await
                }
                Some(c) => {
                    self.buffer(c);
                    self.decimal();
                }
                None => self.yield_single(co, Number(0), row, col).await,
            },
            '1'..='9' => {
                let result = self.decimal();
                self.yield_single(co, result, row, col).await
            }
            c if c.is_whitespace() => (),
            _ => {
                self.yield_single(
                    co,
                    Error(PreprocError::LexError { chr: c }),
                    row,
                    col,
                )
                .await
            }
        }
        DoNothing
    }

    fn ident(&mut self, c: char) -> Token {
        self.buffer(c);

        Token::Ident(self.take_while(char::is_alphanumeric))
    }

    fn number_base(&mut self, base: u32) -> Token {
        let buffer = self.take_while(char::is_alphanumeric);

        let result: Result<u64, _> = u64::from_str_radix(&buffer, base);

        match result {
            Err(_) => Token::Error(PreprocError::InvalidBaseNum {
                s: buffer,
                base: 10,
            }),
            Ok(n) if n > std::u32::MAX as u64 => {
                Token::Error(PreprocError::Overflow { n })
            }
            Ok(n) => Token::Number(n as u32),
        }
    }

    fn hexadecimal(&mut self) -> Token {
        self.number_base(16)
    }

    fn binary(&mut self) -> Token {
        self.number_base(2)
    }

    fn decimal(&mut self) -> Token {
        self.number_base(10)
    }

    fn quoted_string(&mut self) -> Token {
        let mut result = "\"".to_string();

        while let Some(c) = self.next_char_only() {
            match c {
                '"' => {
                    result.push('"');
                    match snailquote::unescape(result.as_str()) {
                        Ok(s) => return Token::QuotedString(s),
                        Err(e) => return Token::Error(PreprocError::Unescape(e)),
                    }
                }
                '\\' => match self.next_char_only() {
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => {
                        return Token::Error(PreprocError::UnclosedQuote);
                    }
                },
                '\n' => {
                    return Token::Error(PreprocError::UnclosedQuote);
                }
                _ => result.push(c),
            }
        }

        Token::Error(PreprocError::UnclosedQuote)
    }
}

pub fn preprocess(
    fname: String,
    stream: impl Iterator<Item = char> + 'static,
    enable_preprocessor: bool,
) -> Gen<TokenAnnot, (), impl std::future::Future<Output = ()>> {
    use PreprocError::*;
    use Token::*;
    let mut state = State::new();
    let stream = FilePosStream::new(fname, stream);
    let mut start_of_line = true;

    let mut stream = StreamStack::new(stream);

    Gen::new(|co| async move {
        while let Some(FilePosAnnot {
            value,
            fname: _,
            row,
            col,
        }) = stream.next()
        {
            if let '\x02' = value {
                if !state.ifstack.is_empty() {
                    stream
                        .yield_single(
                            &co,
                            Error(PreprocError::DirectiveError(
                                directive::DirectiveError::IncludeOpenIf,
                            )),
                            row,
                            0,
                        )
                        .await;
                    state.ifstack.drain(..);
                }
                stream
                    .yield_single(
                        &co,
                        Error(PreprocError::DirectiveError(
                            directive::DirectiveError::IncludeOpenComment,
                        )),
                        row,
                        0,
                    )
                    .await;
                stream.yield_single(&co, Break, row, col).await;
                start_of_line = true;
                continue;
            }
            if let '\x01' = value {
                if !state.ifstack.is_empty() {
                    stream
                        .yield_single(
                            &co,
                            Error(PreprocError::DirectiveError(
                                directive::DirectiveError::IncludeOpenIf,
                            )),
                            row,
                            0,
                        )
                        .await;
                    state.ifstack.drain(..);
                }
                stream.yield_single(&co, Break, row, col).await;
                start_of_line = true;
                continue;
            }

            if value.is_whitespace() && value != '\n' {
                continue;
            }

            if enable_preprocessor && start_of_line && value == '#' {
                let d = StreamStack::take_while(&mut stream, char::is_alphanumeric);

                // TODO: enforce that #include'd files are self-contained
                match directive::process(d, &mut stream, &mut state) {
                    Err(e) => {
                        stream
                            .yield_single(&co, Error(DirectiveError(e)), row, col)
                            .await
                    }
                    Ok(Some(p)) => stream.push(p),
                    Ok(None) => (),
                }

                stream.to_end_of_line();
                continue;
            }

            if !state.active() {
                continue;
            }

            match stream.dispatch(&co, value, row, col).await {
                DispatchResult::DoNothing => start_of_line = false,
                DispatchResult::StartOfLine => start_of_line = true,
            }
        }
    })
}
