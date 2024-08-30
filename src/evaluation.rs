use crate::parser::{Expression, LiteralValue};
use crate::{parser, SourceLoc, WithSourceLoc};
use derive_more::{Display, From};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Default, Debug, Display, From, PartialEq)]
pub enum Value {
    #[display("nil")]
    #[default]
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl From<LiteralValue<'_>> for Value {
    fn from(value: LiteralValue<'_>) -> Self {
        match value {
            LiteralValue::Number(val) => Value::Number(val),
            LiteralValue::String(val) => Value::String(val.to_string()),
            LiteralValue::Boolean(val) => Value::Boolean(val),
            LiteralValue::Nil => Value::Nil,
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        // Lox uses truthyness, not strictly typed booleans
        match value {
            Value::Nil => false,
            Value::Boolean(value) => value,
            Value::Number(_) => true,
            Value::String(_) => true,
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = TypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(value) => Ok(value),
            _ => Err(TypeError::new("number", value)),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Type Error, expected {expected}, got {value}")]
pub struct TypeError {
    expected: &'static str,
    value: Value,

    #[label("here")]
    span: Option<SourceSpan>,

    #[source_code]
    src: Option<String>,
}

impl TypeError {
    pub fn new(expected: &'static str, value: Value) -> Self {
        TypeError {
            expected,
            value,
            span: None,
            src: None,
        }
    }
}

impl WithSourceLoc for TypeError {
    type Wrapped = miette::Error;

    fn with_source_loc(mut self, loc: &crate::SourceLoc) -> Self::Wrapped {
        self.src = Some(loc.source.to_string());
        self.span = Some(loc.into());
        self.into()
    }
}

pub fn evaluate(expr: Expression) -> miette::Result<Value> {
    let val = match expr {
        Expression::Literal(parser::Literal { value, .. }) => value.into(),
        Expression::Grouping(parser::Grouping { expr, .. }) => return evaluate(*expr),
        Expression::Unary(parser::Unary { op, expr, origin }) => {
            let value = evaluate(*expr)?;
            match op {
                parser::UnaryOp::Negate => {
                    let value: f64 = value.try_into().map_err(|err: TypeError| err.with_source_loc(&origin))?;
                    (-value).into()
                }
                parser::UnaryOp::Not => (!bool::from(value)).into(),
            }
        }
        _ => return Err(miette::diagnostic! {
            "Expression type NYI"
        }
        .with_source_loc(expr.origin())),
    };

    Ok(val)
}
