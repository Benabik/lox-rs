use crate::{
    lex::{Token, TokenKind},
    Lexer, SourceLoc,
};
use derive_more::{Display, From};
use miette::{Context, Diagnostic, Report, SourceSpan};
use thiserror::Error;

#[derive(Clone, Display, Debug, From, PartialEq)]
pub enum Expression<'de> {
    Literal(Literal<'de>),
}

#[derive(Clone, Display, Debug, PartialEq)]
#[display("{value}")]
pub struct Literal<'de> {
    value: LiteralValue<'de>,
    origin: SourceLoc<'de>,
}

impl<'de> Literal<'de> {
    pub fn new<T: Into<LiteralValue<'de>>>(value: T, origin: SourceLoc<'de>) -> Self {
        Literal {
            value: value.into(),
            origin,
        }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum LiteralValue<'de> {
    Number(f64),
    String(&'de str),
    Boolean(bool),
    Nil,
}

impl Display for LiteralValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Number(value) => {
                // Not sure how else to ensure a precision of >= 1 instead of exactly 1.
                if value.fract() != 0.0 {
                    write!(f, "{value}")
                } else {
                    write!(f, "{value}.0")
                }
            }
            LiteralValue::String(value) => write!(f, "{value}"),
            LiteralValue::Boolean(value) => write!(f, "{value}"),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct UnexpectedEOFError {
    #[source_code]
    src: String,

    #[label = "EOF"]
    span: SourceSpan,
}

impl UnexpectedEOFError {
    fn new<T: ToString>(source: T) -> Report {
        let src = source.to_string();
        let span = SourceSpan::new(src.len().into(), 0);
        Self { src, span }.into()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token {kind:?}")]
pub struct UnexpectedTokenError {
    kind: TokenKind,

    #[source_code]
    src: String,

    #[label = "this token"]
    span: SourceSpan,
}

impl UnexpectedTokenError {
    fn new(kind: TokenKind, origin: SourceLoc<'_>) -> Self {
        Self {
            kind,
            src: origin.source.to_string(),
            span: origin.into(),
        }
    }

    fn err<T>(self) -> miette::Result<T> {
        Err(self.into())
    }
}

impl From<Token<'_>> for UnexpectedTokenError {
    fn from(token: Token<'_>) -> Self {
        Self::new(token.kind, token.origin)
    }
}

pub struct Parser<'de> {
    lexer: &'de mut Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(lexer: &'de mut Lexer<'de>) -> Self {
        Parser { lexer }
    }

    pub fn expression(&mut self) -> miette::Result<Expression<'de>> {
        self.expression_bp(0)
    }

    fn expression_bp(&mut self, _min_bp: u8) -> miette::Result<Expression<'de>> {
        let Token { text, kind, origin } = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on lhs of expression"),
            None => {
                return Err(UnexpectedEOFError::new(self.lexer.source()))
                    .wrap_err("on lhs of expression")
            }
        };
        let lhs: Expression<'de> = match kind {
            TokenKind::NUMBER => {
                let value: f64 = text.parse().expect("valid from parsing");
                Literal::new(value, origin).into()
            }
            TokenKind::STRING => Literal::new(text, origin).into(),
            TokenKind::TRUE => Literal::new(true, origin).into(),
            TokenKind::FALSE => Literal::new(false, origin).into(),
            TokenKind::NIL => Literal::new(LiteralValue::Nil, origin).into(),
            _ => {
                return UnexpectedTokenError::new(kind, origin)
                    .err()
                    .wrap_err("in lhs of expression")
            }
        };

        match self.lexer.next() {
            None => Ok(lhs),
            Some(Ok(token)) => UnexpectedTokenError::from(token)
                .err()
                .wrap_err("infix expressions NYI"),
            Some(Err(e)) => Err(e).wrap_err("at infix of expression"),
        }
    }
}
