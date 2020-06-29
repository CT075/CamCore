use genawaiter::{
    rc::{gen, Gen},
    yield_,
};
/*
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref NUM_REGEX: Regex =
        Regex::new(r"([01]+b|0x[\da-fA-F]+|\$[\da-fA-F]+|\d+)").unwrap();
    static ref ID_REGEX: Regex = Regex::new("([a-zA-Z_][a-zA-Z0-9_]*)").unwrap();
    static ref WORD_REGEX: Regex = Regex::new("([\\s]+)").unwrap();
}
*/

#[derive(Debug)]
pub struct TokenAnnot {
    tok: Token,
    row: usize,
    col: usize,
    // todo: there's no reason that these need to be owned
    file_name: String,
}

#[derive(Debug)]
pub enum Token {
    Ident(String),
    Number(i32),
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
    Error(String),
}

struct Lexer {
    file_name: String,
    lines: Vec<(usize, String)>,
}

struct Tokenizer {
    file_name: String,
    comment_nesting: usize,
    line: Vec<char>,
    row: usize,
    col: usize,
    finished: bool,
}

fn peek(line: &Vec<char>, col: usize) -> Option<char> {
    if col + 1 >= line.len() {
        return None;
    }
    Some(line[col + 1])
}

fn make_token(tok: Token, file_name: &String, row: usize, col: usize) -> TokenAnnot {
    TokenAnnot {
        tok,
        file_name: file_name.clone(),
        row,
        col,
    }
}

fn string_body(line: &Vec<char>, start: usize) -> Option<(String, usize)> {
    let mut result = String::new();
    let mut col = start;

    while col < line.len() {
        let next = line[col];
        col += 1;
        match next {
            '"' => return Some((result, col)),
            '\\' => {
                result.push(next);
                if let Some(c) = peek(&line, col) {
                    col += 1;
                    result.push(c);
                } else {
                    return None;
                }
            }
            _ => result.push(next),
        }
    }
    None
}

// This is specific to hexnums because binary numbers look different
fn tokenize_hexnum(
    mut buf: String,
    line: &Vec<char>,
    file_name: &String,
    row: usize,
    col: usize,
) -> Option<(TokenAnnot, usize)> {
    let st = col;
    let mut col = col;
    loop {
        if col >= line.len() {
            let t = match i32::from_str_radix(&buf, 16) {
                Ok(i) => Token::Number(i),
                // this case should be impossible
                Err(e) => Token::Error(format!(
                    "could not parse base-16 literal: {}",
                    e.to_string()
                )),
            };
            return Some((make_token(t, file_name, row, st), col));
        }

        let next = line[col];
        if !next.is_alphanumeric() {
            let t = match i32::from_str_radix(&buf, 16) {
                Ok(i) => Token::Number(i),
                // this case should be impossible
                Err(e) => Token::Error(e.to_string().clone()),
            };
            return Some((make_token(t, file_name, row, st), col));
        }

        buf.push(next);
        col += 1;
    }
}

fn dispatch(
    line: &Vec<char>,
    file_name: &String,
    row: usize,
    col: usize,
) -> Option<(TokenAnnot, usize)> {
    let next = line[col];
    let st = col;
    let mut col = col;
    col += 1;
    let mut buf = String::new();

    // If the first character is alphanumeric, assume it's an identifier
    if next.is_alphabetic() {
        buf.push(next);
        loop {
            if col >= line.len() {
                return Some((
                    make_token(Token::Ident(buf), file_name, row, st),
                    line.len(),
                ));
            }

            let next = line[col];
            if !next.is_alphanumeric() {
                return Some((make_token(Token::Ident(buf), file_name, row, st), col));
            }

            buf.push(next);
            col += 1;
        }
    }

    if next == '$' {
        return tokenize_hexnum(buf, &line, &file_name, row, col);
    }

    if next == '0' {
        if let Some('x') = peek(&line, col) {
            col += 1;
            return tokenize_hexnum(buf, &line, &file_name, row, col);
        }
    }

    if next.is_numeric() {
        buf.push(next);
        loop {
            if col >= line.len() {
                let t = match i32::from_str_radix(&buf, 10) {
                    Ok(i) => Token::Number(i),
                    // this case should be impossible
                    Err(e) => Token::Error(format!(
                        "could not parse integer literal: {}",
                        e.to_string()
                    )),
                };
                return Some((make_token(t, &file_name, row, st), col));
            }

            let next = line[col];

            if next == 'b' {
                col += 1;

                let t = match i32::from_str_radix(&buf, 2) {
                    Ok(i) => Token::Number(i),
                    // this case should be impossible
                    Err(e) => Token::Error(format!(
                        "could not parse base-2 literal: {}",
                        e.to_string()
                    )),
                };
                return Some((make_token(t, &file_name, row, st), col));
            }

            if !next.is_alphanumeric() {
                let t = match i32::from_str_radix(&buf, 10) {
                    Ok(i) => Token::Number(i),
                    // this case should be impossible
                    Err(e) => Token::Error(format!(
                        "could not parse integer literal: {}",
                        e.to_string()
                    )),
                };
                return Some((make_token(t, &file_name, row, st), col));
            }

            buf.push(next);
            col += 1;
        }
    }

    if next == '#' {
        return Some((
            make_token(
                Token::Error(format!(concat!(
                    "found hash character (#) in the wrong place; ",
                    "directives go on their own line!"
                ))),
                &file_name,
                row,
                st,
            ),
            col,
        ));
    }

    Some((
        make_token(
            Token::Error(format!("unable to process character {}", next)),
            &file_name,
            row,
            st,
        ),
        col,
    ))
}

impl Lexer {
    fn new(file_name: String, lines: Vec<(usize, String)>) -> Self {
        Lexer { file_name, lines }
    }

    fn tokens(self) -> Gen<TokenAnnot, (), impl std::future::Future> {
        use Token::*;
        let fname = self.file_name;
        let lines = self.lines;
        let mut comment_nesting = 0;
        Gen::new(|co| async move {
            for (row, line) in lines.into_iter() {
                let mut col = 0;
                let line: Vec<char> = line.chars().collect();
                while col < line.len() {
                    let next = line[col];
                    if next.is_whitespace() && next != '\n' {
                        continue;
                    }
                    if comment_nesting > 0 {
                        match next {
                            '*' => {
                                if let Some('/') = peek(&line, col) {
                                    comment_nesting -= 1;
                                    col += 2;
                                }
                            }
                            '/' => {
                                if let Some('/') = peek(&line, col) {
                                    comment_nesting += 1;
                                    col += 2;
                                }
                            }
                            _ => {}
                        }
                        continue;
                    }

                    match next {
                        ';' => co.yield_(make_token(Semi, &fname, row, col)).await,
                        ':' => co.yield_(make_token(Colon, &fname, row, col)).await,
                        '{' => co.yield_(make_token(LCurly, &fname, row, col)).await,
                        '}' => co.yield_(make_token(RCurly, &fname, row, col)).await,
                        '[' => co.yield_(make_token(LBrack, &fname, row, col)).await,
                        ']' => co.yield_(make_token(RBrack, &fname, row, col)).await,
                        '(' => co.yield_(make_token(LParen, &fname, row, col)).await,
                        ')' => co.yield_(make_token(RParen, &fname, row, col)).await,
                        '*' => co.yield_(make_token(Star, &fname, row, col)).await,
                        '%' => co.yield_(make_token(Percent, &fname, row, col)).await,
                        ',' => co.yield_(make_token(Comma, &fname, row, col)).await,
                        '/' => match peek(&line, col) {
                            Some('/') => break,
                            Some('*') => {
                                col += 2;
                                comment_nesting += 1;
                                continue;
                            }
                            _ => co.yield_(make_token(Slash, &fname, row, col)).await,
                        },
                        '+' => co.yield_(make_token(Plus, &fname, row, col)).await,
                        '-' => co.yield_(make_token(Dash, &fname, row, col)).await,
                        '&' => co.yield_(make_token(Ampersand, &fname, row, col)).await,
                        '^' => co.yield_(make_token(Caret, &fname, row, col)).await,
                        '|' => co.yield_(make_token(Bar, &fname, row, col)).await,
                        '"' => {
                            if let Some((s, col_)) = string_body(&line, col + 1) {
                                co.yield_(make_token(
                                    QuotedString(s),
                                    &fname,
                                    row,
                                    col,
                                ))
                                .await;
                                col = col_;
                            } else {
                                co.yield_(make_token(
                                    Error("found an unclosed string".to_string()),
                                    &fname,
                                    row,
                                    line.len(),
                                ))
                                .await;
                                break;
                            }
                        }
                        '<' => {
                            if let Some('<') = peek(&line, col) {
                                co.yield_(make_token(LShift, &fname, row, col)).await;
                                col += 1;
                            } else {
                                co.yield_(make_token(LAngle, &fname, row, col)).await;
                            }
                        }
                        '>' => {
                            if let Some('>') = peek(&line, col) {
                                co.yield_(make_token(RShift, &fname, row, col)).await;
                                col += 1;
                            } else {
                                co.yield_(make_token(RAngle, &fname, row, col)).await;
                            }
                        }
                        // This shouldn't happen, I think?
                        '\n' => co.yield_(make_token(Break, &fname, row, col)).await,
                        //'#' => co.yield_(make_token(Hash, &fname, row, col)).await,
                        '#' => {
                            co.yield_(make_token(
                                Token::Error(format!(concat!(
                                    "found an unexpected hash character (#); ",
                                    "directives go on their own line!"
                                ))),
                                &fname,
                                row,
                                col,
                            ))
                            .await
                        }
                        _ => {
                            if let Some((result, col_)) =
                                dispatch(&line, &fname, row, col - 1)
                            {
                                co.yield_(result).await;
                                col = col_;
                            }
                        }
                    }
                    col += 1;
                }
                co.yield_(make_token(Break, &fname, row, line.len())).await;
            }
        })
    }
}
