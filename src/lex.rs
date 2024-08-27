use miette::MietteDiagnostic;
use std::fmt::Display;

use super::{SourceLoc, WithSourceLoc};

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
}

impl TokenKind {
    fn to_str(&self, _text: &str) -> String {
        match self {
            _ => "null",
        }
        .into()
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
        // Skip whitespace
        let rest = self.rest.trim_start();
        self.advance(self.rest.len() - rest.len());

        // Consider first character
        let mut chars = rest.chars();
        let c = chars.next()?;

        enum Started {
            Single(TokenKind),
            Err(MietteDiagnostic),
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


            _ => Started::Err(miette::diagnostic!("unexpected character {c:?}")),
        };

        // Common case
        let c_len = c.len_utf8();
        let c_str = &self.rest[..c_len];
        self.advance(c_len);

        match started {
            Started::Single(kind) => {
                Some(Ok(Token {
                    text: c_str,
                    kind,
                    origin: self.source_loc(c_len),
                }))
            },
            Started::Err(diag) => {
                Some(Err(diag
                        .with_source_loc(self.source_loc(c_len))
                        .into()))
            }
        }
    }
}
