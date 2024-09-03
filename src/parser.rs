use crate::{
    lex::{Token, TokenKind},
    Lexer, SourceLoc, WithSourceLoc,
};
use derive_more::{Display, From};
use miette::{Context, Diagnostic, Report, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug, From, PartialEq)]
pub struct Program<'de>(pub Vec<Declaration<'de>>);

impl Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut iter = self.0.iter();
        if let Some(fst) = iter.next() {
            write!(f, "{fst}")?;
            for i in iter {
                write!(f, " {i}")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Declaration<'de> {
    Declaration(&'de str, Option<Expression<'de>>),
    Statement(Statement<'de>),
}

impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Declaration(name, expr) => {
                write!(f, "(var {}", name)?;
                if let Some(expr) = expr {
                    write!(f, " {expr}")?;
                }
                write!(f, ")")
            }
            Declaration::Statement(expr) => expr.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'de> {
    Expression(Expression<'de>),
    Print(Expression<'de>),
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self {
            Statement::Expression(e) => e.fmt(f)?,
            Statement::Print(e) => {
                write!(f, "print ")?;
                e.fmt(f)?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Clone, Display, Debug, From, PartialEq)]
pub enum Expression<'de> {
    #[display("{value}")]
    Literal {
        value: LiteralValue<'de>,
        origin: SourceLoc<'de>,
    },

    #[display("({op} {expr})")]
    Unary {
        op: UnaryOp,
        expr: Box<Expression<'de>>,
        origin: SourceLoc<'de>,
    },

    #[display("({op} {lhs} {rhs})")]
    Binary {
        op: BinaryOp,
        lhs: Box<Expression<'de>>,
        rhs: Box<Expression<'de>>,
        origin: SourceLoc<'de>,
    },

    #[display("(group {expr})")]
    Grouping {
        expr: Box<Expression<'de>>,
        origin: SourceLoc<'de>,
    },

    #[display("{name}")]
    Variable {
        name: &'de str,
        origin: SourceLoc<'de>,
    },
}

impl<'de> Expression<'de> {
    pub fn origin(&self) -> &SourceLoc<'de> {
        match self {
            Expression::Literal { origin, .. } => origin,
            Expression::Unary { origin, .. } => origin,
            Expression::Binary { origin, .. } => origin,
            Expression::Grouping { origin, .. } => origin,
            Expression::Variable { origin, .. } => origin,
        }
    }
}

impl<'de> Expression<'de> {
    pub fn literal<T: Into<LiteralValue<'de>>>(value: T, origin: SourceLoc<'de>) -> Self {
        Expression::Literal {
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
pub enum UnaryOp {
    #[display("-")]
    Negate,
    #[display("!")]
    Not,
}

impl UnaryOp {
    pub fn prefix_binding_power(&self) -> u8 {
        11 // One more than BinaryOp::Divide
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
    #[display("=")]
    Assign,
}

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::EQUAL_EQUAL => Ok(Self::Equal),
            TokenKind::BANG_EQUAL => Ok(Self::NotEqual),
            TokenKind::LESS => Ok(Self::Less),
            TokenKind::LESS_EQUAL => Ok(Self::LessEqual),
            TokenKind::GREATER => Ok(Self::Greater),
            TokenKind::GREATER_EQUAL => Ok(Self::GreaterEqual),
            TokenKind::PLUS => Ok(Self::Plus),
            TokenKind::MINUS => Ok(Self::Minus),
            TokenKind::STAR => Ok(Self::Multiply),
            TokenKind::SLASH => Ok(Self::Divide),
            TokenKind::EQUAL => Ok(Self::Assign),
            _ => Err(()),
        }
    }
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOp::Assign => (2, 1),
            BinaryOp::Equal | BinaryOp::NotEqual => (3, 4),
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                (5, 6)
            }
            BinaryOp::Plus | BinaryOp::Minus => (7, 8),
            BinaryOp::Multiply | BinaryOp::Divide => (9, 10),
            // UnaryOp goes here
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
        let span = SourceSpan::new((src.len() - 1).into(), 1);
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

    pub fn peek_for(&mut self, wanted: TokenKind) -> bool {
        match self.lexer.peek() {
            Some(Ok(Token { kind, .. })) if kind == &wanted => true,
            _ => false,
        }
    }

    pub fn expect(&mut self, expect: TokenKind) -> miette::Result<Token<'de>> {
        let expecting = || format!("expecting {expect:?}");
        match self.lexer.next() {
            None => Err(UnexpectedEOFError::new(self.lexer.source())).wrap_err_with(expecting),
            Some(Err(e)) => Err(e).wrap_err_with(expecting),
            Some(Ok(token)) => {
                if token.kind == expect {
                    Ok(token)
                } else {
                    Err(
                        miette::diagnostic!("expecting {expect:?}, found {:?}", token.kind)
                            .with_source_loc(&token.origin),
                    )
                }
            }
        }
    }

    pub fn expect_eof(&mut self) -> miette::Result<()> {
        match self.lexer.next() {
            None => Ok(()),
            Some(Err(e)) => Err(e).wrap_err("expecting EOF"),
            Some(Ok(Token { kind, origin, .. })) => {
                Err(miette::diagnostic!("expecting EOF, found {kind:?}").with_source_loc(&origin))
            }
        }
    }

    pub fn program(&mut self) -> miette::Result<Program<'de>> {
        let mut statements = Vec::new();
        while self.lexer.peek().is_some() {
            statements.push(self.declaration().wrap_err("in program")?);
        }
        Ok(statements.into())
    }

    pub fn declaration(&mut self) -> miette::Result<Declaration<'de>> {
        if self.peek_for(TokenKind::VAR) {
            self.lexer.next(); // Discard VAR
            let var = self.expect(TokenKind::IDENTIFIER)?;
            let expr = if self.peek_for(TokenKind::EQUAL) {
                self.lexer.next(); // Discard EQUAL
                Some(self.expression()?)
            } else {
                None
            };
            self.expect(TokenKind::SEMICOLON)?;
            Ok(Declaration::Declaration(var.text, expr))
        } else {
            Ok(Declaration::Statement(
                self.statement().wrap_err("in declaration")?,
            ))
        }
    }

    pub fn statement(&mut self) -> miette::Result<Statement<'de>> {
        let statement = if self.peek_for(TokenKind::PRINT) {
            self.lexer.next(); // Discard PRINT
            Statement::Print(self.expression().wrap_err("in print statement")?)
        } else {
            Statement::Expression(self.expression().wrap_err("in expression statement")?)
        };
        self.expect(TokenKind::SEMICOLON)?;
        Ok(statement)
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
                Expression::literal(value, origin)
            }
            TokenKind::STRING => Expression::literal(text.trim_matches('"'), origin),
            TokenKind::TRUE => Expression::literal(true, origin),
            TokenKind::FALSE => Expression::literal(false, origin),
            TokenKind::NIL => Expression::literal(LiteralValue::Nil, origin),
            TokenKind::LEFT_PAREN => {
                let expr = self.expression().wrap_err("in parentheses")?;
                self.expect(TokenKind::RIGHT_PAREN)?;
                Expression::Grouping {
                    expr: Box::new(expr),
                    origin,
                }
            }
            TokenKind::IDENTIFIER => Expression::Variable { name: text, origin },
            _ => {
                if let Ok(op) = UnaryOp::try_from(kind) {
                    let expr = self.expression_bp(op.prefix_binding_power())?;
                    Expression::Unary {
                        op,
                        expr: Box::new(expr),
                        origin,
                    }
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
                        lhs = Expression::Binary {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                            origin,
                        };
                        continue;
                    }

                    break;
                }
            }
        }

        Ok(lhs)
    }
}
