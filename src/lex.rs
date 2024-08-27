use miette::{Diagnostic, Report, SourceSpan};
use std::fmt::Display;
use thiserror::Error;

use crate::{SourceLoc, WithSourceLoc};

/// The basic kind of a [Token] for matching
///
/// There is no EOF, as that is just None from [Lexer::next]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    STAR,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    SLASH,
    STRING,
    NUMBER,
    IDENTIFIER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}

impl TokenKind {
    fn to_str(&self, text: &str) -> String {
        match self {
            TokenKind::NUMBER => {
                let value: f64 = text.parse().expect("Lexing should ensure valid number");
                // Not sure how else to ensure a precision of >= 1 instead of exactly 1.
                if value.fract() != 0.0 {
                    format!("{value}")
                } else {
                    format!("{value}.0")
                }
            }
            TokenKind::STRING => text.trim_matches('"').into(),
            _ => "null".into(),
        }
    }
}

/// A lexed token
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Token<'de> {
    kind: TokenKind,
    text: &'de str,
    origin: SourceLoc<'de>,
}

impl<'de> Display for Token<'de> {
    /// Formatted as per test requirements
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {} {}",
            self.kind,
            self.text,
            self.kind.to_str(self.text)
        )
    }
}

/// Makes it easy to get a line number for errors.
pub trait ErrorLine {
    fn try_line(&self) -> Option<usize>;

    fn line(&self) -> usize {
        self.try_line().unwrap_or(0)
    }
}

impl<T: miette::Diagnostic> ErrorLine for T {
    fn try_line(&self) -> Option<usize> {
        let span = self.labels()?.next()?;
        let contents = self.source_code()?.read_span(span.inner(), 0, 0).ok()?;
        Some(contents.line() + 1)
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("[line {}] Error: Unexpected character: {c}", self.line())]
pub struct UnexpectedCharError {
    pub c: char,

    #[source_code]
    src: String,

    #[label = "here"]
    span: SourceSpan,
}

impl UnexpectedCharError {
    fn new(c: char, loc: SourceLoc) -> Report {
        Self {
            c,
            src: loc.source.to_string(),
            span: SourceSpan::new(loc.offset.into(), loc.len),
        }
        .into()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("[line {}] Error: Unterminated string.", self.line())]
pub struct UnterminatedStringError {
    #[source_code]
    src: String,

    #[label = "here"]
    span: SourceSpan,
}

impl UnterminatedStringError {
    fn new(loc: SourceLoc) -> Report {
        Self {
            src: loc.source.to_string(),
            span: SourceSpan::new(loc.offset.into(), loc.len),
        }
        .into()
    }
}

/// Holds the state of lexing source code
pub struct Lexer<'de> {
    source: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(source: &'de str) -> Self {
        Lexer {
            source,
            rest: source,
            byte: 0,
        }
    }

    fn source_loc(&self, len: usize) -> SourceLoc<'de> {
        SourceLoc {
            source: self.source,
            offset: self.byte - len,
            len,
        }
    }

    fn advance(&mut self, len: usize) {
        self.rest = &self.rest[len..];
        self.byte += len;
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = miette::Result<Token<'de>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Mostly to allow looping after a comment
            // Skip whitespace
            let rest = self.rest.trim_start();
            self.advance(self.rest.len() - rest.len());

            // Consider first character
            let mut chars = rest.chars();
            let c = chars.next()?;
            let c_len = c.len_utf8();

            enum Started {
                Single(TokenKind),
                Equals(TokenKind, TokenKind),
                String,
                Number,
                Identifier,
            }

            let started = match c {
                // Unambiguous single character tokens
                '(' => Started::Single(TokenKind::LEFT_PAREN),
                ')' => Started::Single(TokenKind::RIGHT_PAREN),
                '{' => Started::Single(TokenKind::LEFT_BRACE),
                '}' => Started::Single(TokenKind::RIGHT_BRACE),
                ',' => Started::Single(TokenKind::COMMA),
                '.' => Started::Single(TokenKind::DOT),
                '-' => Started::Single(TokenKind::MINUS),
                '+' => Started::Single(TokenKind::PLUS),
                ';' => Started::Single(TokenKind::SEMICOLON),
                '*' => Started::Single(TokenKind::STAR),

                // Comparison operators (may be followed by =)
                '<' => Started::Equals(TokenKind::LESS, TokenKind::LESS_EQUAL),
                '>' => Started::Equals(TokenKind::GREATER, TokenKind::GREATER_EQUAL),
                '!' => Started::Equals(TokenKind::BANG, TokenKind::BANG_EQUAL),
                '=' => Started::Equals(TokenKind::EQUAL, TokenKind::EQUAL_EQUAL),

                // Longer tokens
                '"' => Started::String,
                '0'..='9' => Started::Number,

                '_' => Started::Identifier,
                c if c.is_ascii_alphabetic() => Started::Identifier,

                // Maybe comment: special case here to reuse Single machinery later
                '/' => {
                    if let Some('/') = chars.next() {
                        // Advance until newline...
                        let line_end = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.advance(line_end);
                        continue;
                    } else {
                        Started::Single(TokenKind::SLASH)
                    }
                }

                _ => {
                    self.advance(c_len);
                    return Some(Err(UnexpectedCharError::new(c, self.source_loc(c_len))));
                }
            };

            let token = match started {
                Started::Single(kind) => {
                    let text = &self.rest[..c_len];
                    self.advance(c_len);

                    Token {
                        text,
                        kind,
                        origin: self.source_loc(c_len),
                    }
                }
                Started::Equals(single, equals) => {
                    let (len, kind) = if let Some('=') = chars.next() {
                        (c_len + 1, equals)
                    } else {
                        (c_len, single)
                    };

                    let text = &self.rest[..len];
                    self.advance(len);

                    Token {
                        text,
                        kind,
                        origin: self.source_loc(len),
                    }
                }
                Started::String => {
                    if let Some(end) = self.rest[1..].find('"') {
                        let len = end + 2; // Need to add both quotes
                        let text = &self.rest[..len];
                        self.advance(len);
                        Token {
                            text,
                            kind: TokenKind::STRING,
                            origin: self.source_loc(len),
                        }
                    } else {
                        // Get location of quote
                        self.advance(1);
                        let loc = self.source_loc(1);

                        // Consume rest of source
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(UnterminatedStringError::new(loc)));
                    }
                }
                Started::Number => {
                    // Find length of number by finding end of numbers
                    let len = self
                        .rest
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or(self.rest.len());
                    let mut text = &self.rest[..len];

                    // Use jonhoo's solution for removing excess .
                    let mut dotted = text.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            text = &text[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            text = &text[..one.len()];
                        }
                        _ => { /* leave text as-is */ }
                    }

                    let len = text.len();
                    self.advance(len);
                    let origin = self.source_loc(len);

                    // This might be paranoid, as I think anything that gets here is a valid number
                    if text.parse::<f64>().is_err() {
                        return Some(Err(
                            miette::diagnostic!("malformed number").with_source_loc(origin)
                        ));
                    }

                    Token {
                        text,
                        kind: TokenKind::NUMBER,
                        origin,
                    }
                }
                Started::Identifier => {
                    // Find length of identifier by finding end of valid characters
                    let len = self
                        .rest
                        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                        .unwrap_or(self.rest.len());
                    let text = &self.rest[..len];
                    self.advance(len);
                    let origin = self.source_loc(len);

                    // Check for keywords
                    let kind = match text {
                        "and" => TokenKind::AND,
                        "class" => TokenKind::CLASS,
                        "else" => TokenKind::ELSE,
                        "false" => TokenKind::FALSE,
                        "for" => TokenKind::FOR,
                        "fun" => TokenKind::FUN,
                        "if" => TokenKind::IF,
                        "nil" => TokenKind::NIL,
                        "or" => TokenKind::OR,
                        "print" => TokenKind::PRINT,
                        "return" => TokenKind::RETURN,
                        "super" => TokenKind::SUPER,
                        "this" => TokenKind::THIS,
                        "true" => TokenKind::TRUE,
                        "var" => TokenKind::VAR,
                        "while" => TokenKind::WHILE,
                        _ => TokenKind::IDENTIFIER,
                    };
                    Token { text, kind, origin }
                }
            };
            return Some(Ok(token));
        }
    }
}
