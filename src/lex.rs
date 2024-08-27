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
        let c_len = c.len_utf8();
        let text = &self.rest[..c_len];

        self.advance(c_len));

        let error = |diag: MietteDiagnostic| {
            Some(Err(diag
                    .with_source_loc(self.source_loc(text.len()))
                    .into()))
        };
        let token = |kind: TokenKind| {
            Some(Ok(Token {
                text,
                kind,
                origin: self.source_loc(text.len()),
            }))
        };

        return match c {
            '(' => token(TokenKind::LEFT_PAREN),
            ')' => token(TokenKind::RIGHT_PAREN),
            _ => error(miette::diagnostic!("unexpected character {c:?}")),
        };
    }
}
