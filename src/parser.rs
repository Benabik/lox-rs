use crate::{
    lex::{Token, TokenKind},
    Lexer, SourceLoc, WithSourceLoc,
};
use derive_more::{Display, From};
use miette::{Context, Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug, From, PartialEq)]
pub struct Block<'de>(pub Vec<Declaration<'de>>);

impl<'de> Block<'de> {
    fn into_statement(self, origin: SourceLoc<'de>) -> Statement<'de> {
        Statement::Block { body: self, origin }
    }
}

impl Display for Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(block")?;
        for i in &self.0 {
            write!(f, " {i}")?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'de> {
    pub name: &'de str,
    pub arguments: Vec<&'de str>,
    pub body: Block<'de>,
    pub origin: SourceLoc<'de>,
    pub initializer: bool,
}

impl Display for Function<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Function {
            name,
            arguments,
            body,
            ..
        } = self;
        write!(f, "(fun {name} (")?;
        let mut iter = arguments.iter();
        if let Some(first) = iter.next() {
            first.fmt(f)?;
        }
        for i in iter {
            write!(f, " {i}")?;
        }
        write!(f, ") {body})")
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Declaration<'de> {
    Class {
        name: &'de str,
        superclass: Option<&'de str>,
        methods: Vec<Function<'de>>,
        origin: SourceLoc<'de>,
    },
    #[from]
    Function(Function<'de>),
    #[from(forward)]
    Statement(Statement<'de>),
    Variable {
        name: &'de str,
        init: Option<Expression<'de>>,
        origin: SourceLoc<'de>,
    },
}

impl<'de> Declaration<'de> {
    fn origin(&self) -> &SourceLoc<'de> {
        match self {
            Declaration::Class { origin, .. } | Declaration::Variable { origin, .. } => origin,
            Declaration::Function(function) => &function.origin,
            Declaration::Statement(statement) => statement.origin(),
        }
    }
}

impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Class { name, methods, .. } => {
                write!(f, "(class {}", name)?;
                methods.iter().try_for_each(|m| m.fmt(f))?;
                write!(f, ")")
            }
            Declaration::Function(func) => func.fmt(f),
            Declaration::Variable { name, init, .. } => {
                write!(f, "(var {}", name)?;
                if let Some(expr) = init {
                    write!(f, " {expr}")?;
                }
                write!(f, ")")
            }
            Declaration::Statement(expr) => expr.fmt(f),
        }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Statement<'de> {
    #[from(forward)]
    Block {
        body: Block<'de>,
        origin: SourceLoc<'de>,
    },
    #[from(forward)]
    Expression(Expression<'de>),
    If {
        condition: Expression<'de>,
        then: Box<Statement<'de>>,
        other: Option<Box<Statement<'de>>>,
        origin: SourceLoc<'de>,
    },
    Print(Expression<'de>),
    Return {
        expression: Option<Expression<'de>>,
        origin: SourceLoc<'de>,
    },
    While {
        condition: Expression<'de>,
        body: Box<Statement<'de>>,
        origin: SourceLoc<'de>,
    },
}

impl<'de> Statement<'de> {
    fn origin(&self) -> &SourceLoc<'de> {
        match self {
            Statement::Expression(expression) | Statement::Print(expression) => expression.origin(),
            Statement::Block { origin, .. }
            | Statement::If { origin, .. }
            | Statement::Return { origin, .. }
            | Statement::While { origin, .. } => origin,
        }
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Block { body, .. } => body.fmt(f),
            Statement::Expression(e) => e.fmt(f),
            Statement::If {
                condition,
                then,
                other,
                ..
            } => {
                write!(f, "(if {condition} {then}")?;
                if let Some(other) = other {
                    write!(f, " {other}")?;
                }
                write!(f, ")")
            }
            Statement::Print(e) => write!(f, "(print {e})"),
            Statement::Return {
                expression: Some(e),
                ..
            } => write!(f, "(return {e})"),
            Statement::Return {
                expression: None, ..
            } => write!(f, "(return)"),
            Statement::While {
                condition, body, ..
            } => write!(f, "(while {condition} {body})"),
        }
    }
}

/// Mostly a wrapper for display purposes
#[derive(Clone, Debug, From, PartialEq)]
pub struct Arguments<'de>(pub Vec<Expression<'de>>);

impl Display for Arguments<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.0 {
            write!(f, " {i}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Display, Debug, PartialEq)]
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

    #[display("(assign {name} {expr})")]
    Assign {
        name: &'de str,
        expr: Box<Expression<'de>>,
        origin: SourceLoc<'de>,
    },

    #[display("(assign-prop {object} {name} {expr})")]
    AssignProp {
        object: Box<Expression<'de>>,
        name: &'de str,
        expr: Box<Expression<'de>>,
        origin: SourceLoc<'de>,
    },

    #[display("(call {callee}{arguments})")]
    Call {
        callee: Box<Expression<'de>>,
        arguments: Arguments<'de>,
        origin: SourceLoc<'de>,
    },

    #[display("(prop {object} {name})")]
    Property {
        object: Box<Expression<'de>>,
        name: &'de str,
        origin: SourceLoc<'de>,
    },

    #[display("this")]
    This(SourceLoc<'de>),
}

impl<'de> Expression<'de> {
    pub fn origin(&self) -> &SourceLoc<'de> {
        match self {
            Expression::Literal { origin, .. }
            | Expression::Unary { origin, .. }
            | Expression::Binary { origin, .. }
            | Expression::Grouping { origin, .. }
            | Expression::Variable { origin, .. }
            | Expression::Assign { origin, .. }
            | Expression::AssignProp { origin, .. }
            | Expression::Call { origin, .. }
            | Expression::Property { origin, .. }
            | Expression::This(origin) => origin,
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

#[derive(Copy, Clone, Display, Debug, PartialEq)]
pub enum UnaryOp {
    #[display("-")]
    Negate,
    #[display("!")]
    Not,
}

impl UnaryOp {
    pub fn prefix_binding_power(&self) -> u8 {
        13 // One more than BinaryOp::Divide
           // Property/Call goes here
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

#[derive(Copy, Clone, Display, Debug, PartialEq)]
pub enum BinaryOp {
    #[display("or")]
    Or,
    #[display("and")]
    And,
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
            TokenKind::OR => Ok(Self::Or),
            TokenKind::AND => Ok(Self::And),
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
            _ => Err(()),
        }
    }
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            // Expression::Assign is 2, 1
            BinaryOp::Or => (3, 4),
            BinaryOp::And => (5, 6),
            BinaryOp::Equal | BinaryOp::NotEqual => (5, 6),
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                (7, 8)
            }
            BinaryOp::Plus | BinaryOp::Minus => (9, 10),
            BinaryOp::Multiply | BinaryOp::Divide => (11, 12),
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
    fn new<T: ToString>(source: T) -> Self {
        let src = source.to_string();
        let span = SourceSpan::new((src.len() - 1).into(), 1);
        Self { src, span }
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

#[derive(Diagnostic, Debug, Error)]
#[error("Invalid assignment target.")]
pub struct InvalidAssignmentError {
    #[label("here")]
    span: SourceSpan,

    #[source_code]
    src: String,
}

impl InvalidAssignmentError {
    fn new(origin: &SourceLoc) -> Self {
        Self {
            span: origin.into(),
            src: origin.source.to_string(),
        }
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
        self.peek_kind().map(|kind| kind == wanted).unwrap_or(false)
    }

    pub fn peek_kind(&mut self) -> Option<TokenKind> {
        if let Some(Ok(Token { kind, .. })) = self.lexer.peek() {
            Some(*kind)
        } else {
            None
        }
    }

    /// Used after peek_for/peek_kind, panics if there is no next token
    fn expect_any(&mut self) -> Token<'de> {
        self.lexer
            .next()
            .expect("expected Some")
            .expect("expected Ok")
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

    pub fn program(&mut self) -> miette::Result<Block<'de>> {
        let mut statements = Vec::new();
        while self.lexer.peek().is_some() {
            statements.push(self.declaration().wrap_err("in program")?);
        }
        Ok(statements.into())
    }

    fn block(&mut self) -> miette::Result<(Block<'de>, SourceLoc<'de>)> {
        let Token { origin, .. } = self.expect(TokenKind::LEFT_BRACE).wrap_err("in block")?;
        let mut statements = Vec::new();
        while !self.peek_for(TokenKind::RIGHT_BRACE) {
            statements.push(self.declaration().wrap_err("in block")?);
        }
        self.expect(TokenKind::RIGHT_BRACE).wrap_err("in block")?;
        Ok((statements.into(), origin))
    }

    fn var_declaration(&mut self) -> miette::Result<Declaration<'de>> {
        self.expect(TokenKind::VAR).wrap_err("in var declaration")?;
        let var = self
            .expect(TokenKind::IDENTIFIER)
            .wrap_err("in var declaration")?;
        let expr = if self.peek_for(TokenKind::EQUAL) {
            self.lexer.next(); // Discard EQUAL
            Some(self.expression()?)
        } else {
            None
        };
        self.expect(TokenKind::SEMICOLON)
            .wrap_err("in var declaration")?;
        Ok(Declaration::Variable {
            name: var.text,
            init: expr,
            origin: var.origin,
        })
    }

    pub fn function(&mut self) -> miette::Result<Function<'de>> {
        // NB: fun keyword is not part of function rule
        let name = self
            .expect(TokenKind::IDENTIFIER)
            .wrap_err("in var declaration")?;

        let mut arguments = Vec::new();
        self.expect(TokenKind::LEFT_PAREN)
            .wrap_err("in function declaration")?;
        while !self.peek_for(TokenKind::RIGHT_PAREN) {
            let arg = self
                .expect(TokenKind::IDENTIFIER)
                .wrap_err("in function declaration")?;
            arguments.push(arg.text);

            if self.peek_for(TokenKind::COMMA) {
                self.lexer.next(); // discard COMMA
            } else {
                break;
            }
        }
        self.expect(TokenKind::RIGHT_PAREN)
            .wrap_err("in function declaration")?;

        let (body, _) = self.block().wrap_err("in function declaration")?;
        Ok(Function {
            name: name.text,
            arguments,
            body,
            origin: name.origin,
            initializer: false,
        })
    }

    pub fn declaration(&mut self) -> miette::Result<Declaration<'de>> {
        let ret = match self.peek_kind() {
            Some(TokenKind::CLASS) => {
                self.lexer.next(); // Discard CLASS
                let Token { text, origin, .. } = self.expect(TokenKind::IDENTIFIER)?;

                // Superclass?
                let superclass = if self.peek_for(TokenKind::LESS) {
                    self.expect_any();
                    Some(self.expect(TokenKind::IDENTIFIER)?.text)
                } else {
                    None
                };

                // Class body
                self.expect(TokenKind::LEFT_BRACE)?;
                let mut methods = Vec::new();
                while !self.peek_for(TokenKind::RIGHT_BRACE) {
                    let mut func = self.function()?;
                    if func.name == "init" {
                        func.initializer = true;
                    }
                    methods.push(func);
                }
                self.lexer.next(); // Discard RIGHT_BRACE

                Declaration::Class {
                    name: text,
                    superclass,
                    methods,
                    origin,
                }
            }
            Some(TokenKind::FUN) => {
                self.lexer.next(); // Discard FUN
                Declaration::Function(self.function()?)
            }
            Some(TokenKind::VAR) => self.var_declaration()?,
            _ => Declaration::Statement(self.statement().wrap_err("in declaration")?),
        };
        Ok(ret)
    }

    pub fn statement(&mut self) -> miette::Result<Statement<'de>> {
        let statement = match self.peek_kind() {
            Some(TokenKind::FOR) => {
                let Token { origin, .. } = self.expect_any();
                self.expect(TokenKind::LEFT_PAREN)
                    .wrap_err("in for statement")?;

                let initializer = match self.peek_kind() {
                    Some(TokenKind::VAR) => {
                        Some(self.var_declaration().wrap_err("in for initializer")?)
                    }
                    Some(TokenKind::SEMICOLON) => {
                        self.lexer.next(); // Discard SEMICOLON
                        None
                    }
                    _ => {
                        let expr = self.expression().wrap_err("in for initializer")?;
                        self.expect(TokenKind::SEMICOLON)
                            .wrap_err("in for initializer")?;
                        Some(expr.into())
                    }
                };

                let condition = if self.peek_for(TokenKind::SEMICOLON) {
                    // No condition is infinite loop, so synthesize a true condition
                    let Token { origin, .. } = self.expect_any();
                    Expression::Literal {
                        value: true.into(),
                        origin,
                    }
                } else {
                    let expr = self.expression().wrap_err("in for condition")?;
                    self.expect(TokenKind::SEMICOLON)
                        .wrap_err("in for condition")?;
                    expr
                };

                let increment = if self.peek_for(TokenKind::RIGHT_PAREN) {
                    None
                } else {
                    let expr = self.expression().wrap_err("in for increment")?;
                    Some(expr)
                };

                self.expect(TokenKind::RIGHT_PAREN)
                    .wrap_err("in for statement")?;

                let mut body = self.statement().wrap_err("in for statement")?;

                if let Some(increment) = increment {
                    let origin = increment.origin().clone();
                    body = Block(vec![body.into(), increment.into()]).into_statement(origin);
                }

                body = Statement::While {
                    condition,
                    body: Box::new(body),
                    origin,
                };

                if let Some(initializer) = initializer {
                    let origin = initializer.origin().clone();
                    body = Block(vec![initializer, body.into()]).into_statement(origin);
                }

                body
            }

            Some(TokenKind::IF) => {
                let Token { origin, .. } = self.expect_any();
                self.expect(TokenKind::LEFT_PAREN)
                    .wrap_err("in if statement")?;
                let condition = self.expression().wrap_err("in if condition")?;
                self.expect(TokenKind::RIGHT_PAREN)
                    .wrap_err("in if statement")?;

                // Delay placing in Box until after all parsing complete
                let then = self.statement().wrap_err("in if statement")?;

                let other = if self.peek_for(TokenKind::ELSE) {
                    self.lexer.next(); // Discard ELSE
                    Some(Box::new(self.statement().wrap_err("in else statement")?))
                } else {
                    None
                };
                Statement::If {
                    condition,
                    then: Box::new(then),
                    other,
                    origin,
                }
            }

            Some(TokenKind::LEFT_BRACE) => self.block()?.into(),

            Some(TokenKind::PRINT) => {
                self.lexer.next(); // Discard PRINT
                let expr = self.expression().wrap_err("in print statement")?;
                self.expect(TokenKind::SEMICOLON)
                    .wrap_err("in print statement")?;
                Statement::Print(expr)
            }

            Some(TokenKind::RETURN) => {
                let Token { origin, .. } = self.expect_any();
                let expr = if self.peek_for(TokenKind::SEMICOLON) {
                    None
                } else {
                    Some(self.expression().wrap_err("in return statement")?)
                };
                self.expect(TokenKind::SEMICOLON)
                    .wrap_err("in return statement")?;
                Statement::Return {
                    expression: expr,
                    origin,
                }
            }

            Some(TokenKind::WHILE) => {
                let Token { origin, .. } = self.expect_any(); // Discard WHILE
                let condition = self.expression().wrap_err("in while condition")?;
                let body = Box::new(self.statement().wrap_err("in while body")?);
                Statement::While {
                    condition,
                    body,
                    origin,
                }
            }

            _ => {
                let expr = self.expression().wrap_err("in expression statement")?;
                self.expect(TokenKind::SEMICOLON)
                    .wrap_err("in expression statement")?;
                Statement::Expression(expr)
            }
        };
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
            TokenKind::THIS => Expression::This(origin),
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
            match self.peek_kind() {
                None => break,

                Some(TokenKind::DOT) => {
                    // Highest binding power, no check
                    self.lexer.next(); // Discard DOT
                    let Token { text, origin, .. } = self.expect(TokenKind::IDENTIFIER)?;
                    lhs = Expression::Property {
                        object: Box::new(lhs),
                        name: text,
                        origin,
                    };
                    continue;
                }

                Some(TokenKind::LEFT_PAREN) => {
                    // Highest binding power, no check
                    let Token { origin, .. } = self.expect_any();

                    let mut arguments = Vec::new();
                    while !self.peek_for(TokenKind::RIGHT_PAREN) {
                        arguments.push(self.expression_bp(0)?);

                        if self.peek_for(TokenKind::COMMA) {
                            self.lexer.next(); // discard COMMA
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenKind::RIGHT_PAREN)
                        .wrap_err("in function call")?;

                    lhs = Expression::Call {
                        callee: Box::new(lhs),
                        arguments: arguments.into(),
                        origin,
                    };
                    continue;
                }

                Some(TokenKind::EQUAL) => {
                    // Assignment binding_power
                    let (l_bp, r_bp) = (2, 1);
                    if l_bp < min_bp {
                        break;
                    }

                    let Token { origin, .. } = self.expect_any();

                    let expr = Box::new(self.expression_bp(r_bp)?);

                    lhs = match lhs {
                        // Lift a property read to a property write
                        Expression::Property { object, name, .. } => Expression::AssignProp {
                            object,
                            name,
                            expr,
                            origin,
                        },

                        Expression::Variable { name, .. } => {
                            Expression::Assign { name, expr, origin }
                        }
                        _ => return Err(InvalidAssignmentError::new(&origin).into()),
                    };

                    continue;
                }

                Some(kind) => {
                    let Ok(op) = BinaryOp::try_from(kind) else {
                        break;
                    };
                    let (l_bp, r_bp) = op.binding_power();
                    if l_bp < min_bp {
                        break;
                    }
                    let Token { origin, .. } = self.expect_any();

                    let rhs = self.expression_bp(r_bp)?;
                    lhs = Expression::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        origin,
                    };
                    continue;
                }
            }
        }

        Ok(lhs)
    }
}
