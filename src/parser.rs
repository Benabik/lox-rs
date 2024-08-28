use crate::{
    lex::{Token, TokenKind},
    Lexer, SourceLoc, WithSourceLoc,
};
use derive_more::{Display, From};
use miette::{Context, Diagnostic, Report, SourceSpan};
use thiserror::Error;

#[derive(Clone, Display, Debug, From, PartialEq)]
pub enum Expression<'de> {
    Literal(Literal<'de>),
    Unary(Unary<'de>),
    Binary(Binary<'de>),
    Grouping(Grouping<'de>),
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
            // It seems like you should have to output a quoted value here, but that fails tests
            LiteralValue::String(value) => write!(f, "{value}"),
            LiteralValue::Boolean(value) => write!(f, "{value}"),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone, Display, Debug, PartialEq)]
#[display("({op} {expr})")]
pub struct Unary<'de> {
    op: UnaryOp,
    expr: Box<Expression<'de>>,
    origin: SourceLoc<'de>,
}

#[derive(Clone, Display, Debug, PartialEq)]
pub enum UnaryOp {
    #[display("-")]
    Negate,
    #[display("!")]
    Not,
}

impl UnaryOp {
    pub fn prefix_binding_power(&self) -> u8 {
        9 // One more than BinaryOp::Divide
    }
}

impl TryFrom<TokenKind> for UnaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::BANG => Ok(UnaryOp::Not),
            TokenKind::MINUS => Ok(UnaryOp::Negate),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Display, Debug, PartialEq)]
#[display("({op} {lhs} {rhs})")]
pub struct Binary<'de> {
    op: BinaryOp,
    lhs: Box<Expression<'de>>,
    rhs: Box<Expression<'de>>,
    origin: SourceLoc<'de>,
}

#[derive(Clone, Display, Debug, PartialEq)]
pub enum BinaryOp {
    #[display("==")]
    Equal,
    #[display("!=")]
    NotEqual,
    #[display("<")]
    Less,
    #[display("<=")]
    LessEqual,
    #[display(">")]
    Greater,
    #[display(">=")]
    GreaterEqual,
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Multiply,
    #[display("/")]
    Divide,
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::EQUAL => Ok(Self::Equal),
            TokenKind::BANG_EQUAL => Ok(Self::NotEqual),
            TokenKind::LESS => Ok(Self::Less),
            TokenKind::LESS_EQUAL => Ok(Self::LessEqual),
            TokenKind::GREATER => Ok(Self::Greater),
            TokenKind::GREATER_EQUAL => Ok(Self::GreaterEqual),
            TokenKind::PLUS => Ok(Self::Plus),
            TokenKind::MINUS => Ok(Self::Minus),
            TokenKind::STAR => Ok(Self::Multiply),
            TokenKind::SLASH => Ok(Self::Divide),
            _ => Err(()),
        }
    }
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Equal | BinaryOp::NotEqual => (1, 2),
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                (3, 4)
            }
            BinaryOp::Plus | BinaryOp::Minus => (5, 6),
            BinaryOp::Multiply | BinaryOp::Divide => (7, 8),
            // UnaryOp goes here
        }
    }
}

#[derive(Clone, Display, Debug, PartialEq)]
#[display("(group {expr})")]
pub struct Grouping<'de> {
    expr: Box<Expression<'de>>,
    origin: SourceLoc<'de>,
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

    pub fn expect(&mut self, expect: TokenKind) -> miette::Result<()> {
        let expecting = || format!("expecting {expect:?}");
        match self.lexer.next() {
            None => Err(UnexpectedEOFError::new(self.lexer.source())).wrap_err_with(expecting),
            Some(Err(e)) => Err(e).wrap_err_with(expecting),
            Some(Ok(Token { kind, origin, .. })) => {
                if kind == expect {
                    Ok(())
                } else {
                    Err(miette::diagnostic!("expecting {expect:?}, found {kind:?}")
                        .with_source_loc(origin))
                }
            }
        }
    }

    pub fn expect_eof(&mut self) -> miette::Result<()> {
        match self.lexer.next() {
            None => Ok(()),
            Some(Err(e)) => Err(e).wrap_err("expecting EOF"),
            Some(Ok(Token { kind, origin, .. })) => {
                Err(miette::diagnostic!("expecting EOF, found {kind:?}").with_source_loc(origin))
            }
        }
    }

    pub fn expression(&mut self) -> miette::Result<Expression<'de>> {
        self.expression_bp(0)
    }

    fn expression_bp(&mut self, min_bp: u8) -> miette::Result<Expression<'de>> {
        let Token { text, kind, origin } = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on lhs of expression"),
            None => {
                return Err(UnexpectedEOFError::new(self.lexer.source()))
                    .wrap_err("on lhs of expression")
            }
        };

        let mut lhs = match kind {
            TokenKind::NUMBER => {
                let value: f64 = text.parse().expect("valid from parsing");
                Literal::new(value, origin).into()
            }
            TokenKind::STRING => Literal::new(text.trim_matches('"'), origin).into(),
            TokenKind::TRUE => Literal::new(true, origin).into(),
            TokenKind::FALSE => Literal::new(false, origin).into(),
            TokenKind::NIL => Literal::new(LiteralValue::Nil, origin).into(),
            TokenKind::LEFT_PAREN => {
                let expr = self.expression().wrap_err("in parentheses")?;
                self.expect(TokenKind::RIGHT_PAREN)?;
                Grouping {
                    expr: Box::new(expr),
                    origin,
                }
                .into()
            }
            _ => {
                if let Ok(op) = UnaryOp::try_from(kind) {
                    let expr = self.expression_bp(op.prefix_binding_power())?;
                    Unary {
                        op,
                        expr: Box::new(expr),
                        origin,
                    }
                    .into()
                } else {
                    return UnexpectedTokenError::new(kind, origin)
                        .err()
                        .wrap_err("in lhs of expression");
                }
            }
        };

        loop {
            match self.lexer.peek() {
                None => break,
                Some(Err(_)) => {
                    return Err(self
                        .lexer
                        .next()
                        .expect("checked some in match")
                        .expect_err("check error in match"))
                    .wrap_err("expecting operator of expression");
                }
                Some(Ok(token)) => {
                    // TODO: Check postfix

                    if let Ok(op) = BinaryOp::try_from(token.kind) {
                        let (l_bp, r_bp) = op.binding_power();
                        if l_bp < min_bp {
                            break;
                        }
                        let Token { origin, .. } =
                            self.lexer.next().expect("peeked Some").expect("peeked Ok");

                        let rhs = self.expression_bp(r_bp)?;
                        lhs = Binary {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            origin,
                        }
                        .into();
                        continue;
                    }

                    break;
                }
            }
        }

        Ok(lhs)
    }
}
