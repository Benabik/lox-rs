use crate::parser::{Expression, LiteralValue, Program};
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

impl TryFrom<Value> for String {
    type Error = TypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value),
            _ => Err(TypeError::new("string", value)),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = TypeError;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value),
            _ => Err(TypeError::new("string", value.clone())),
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

#[derive(Clone, Debug, Default)]
pub struct Evaluator {}

impl Evaluator {
    pub fn run(&mut self, prog: Program) -> miette::Result<()> {
        for s in prog.0 {
            use parser::Statement::*;
            match s {
                Expression(e) => {
                    self.expression(e)?;
                }
                Print(e) => {
                    let val = self.expression(e)?;
                    println!("{val}");
                }
            };
        }
        Ok(())
    }

    pub fn expression(&mut self, expr: Expression) -> miette::Result<Value> {
        let to_float = |val: Value, origin: &SourceLoc| f64::try_from(val).with_source_loc(origin);
        let to_string =
            |val: Value, origin: &SourceLoc| String::try_from(val).with_source_loc(origin);

        let val =
            match expr {
                Expression::Literal { value, .. } => value.into(),
                Expression::Grouping { expr, .. } => return self.expression(*expr),
                Expression::Unary { op, expr, origin } => {
                    let value = self.expression(*expr)?;
                    match op {
                        parser::UnaryOp::Negate => (-to_float(value, &origin)?).into(),
                        parser::UnaryOp::Not => (!bool::from(value)).into(),
                    }
                }
                Expression::Binary {
                    op,
                    lhs,
                    rhs,
                    origin,
                } => {
                    let lhs = self.expression(*lhs)?;
                    let rhs = self.expression(*rhs)?;

                    let binary_float = |lhs, rhs, f: fn(f64, f64) -> f64| {
                        let lhs = to_float(lhs, &origin)?;
                        let rhs = to_float(rhs, &origin)?;
                        Ok::<Value, miette::Report>(f(lhs, rhs).into())
                    };

                    use parser::BinaryOp::*;
                    match op {
                        Equal => Value::from(lhs == rhs),
                        NotEqual => Value::from(lhs != rhs),
                        Less | LessEqual | Greater | GreaterEqual => match lhs {
                            Value::Number(lhs) => {
                                let rhs = to_float(rhs, &origin)?;
                                match op {
                                    Less => lhs < rhs,
                                    LessEqual => lhs <= rhs,
                                    Greater => lhs > rhs,
                                    GreaterEqual => lhs >= rhs,
                                    _ => unreachable!("by outer match"),
                                }
                                .into()
                            }
                            Value::String(lhs) => {
                                let rhs = to_string(rhs, &origin)?;
                                match op {
                                    Less => lhs < rhs,
                                    LessEqual => lhs <= rhs,
                                    Greater => lhs > rhs,
                                    GreaterEqual => lhs >= rhs,
                                    _ => unreachable!("by outer match"),
                                }
                                .into()
                            }
                            _ => {
                                return Err(TypeError::new("number or string", lhs)
                                    .with_source_loc(&origin))
                            }
                        },
                        Plus => match lhs {
                            Value::Number(lhs) => (lhs + to_float(rhs, &origin)?).into(),
                            Value::String(mut lhs) => {
                                lhs += &to_string(rhs, &origin)?;
                                lhs.into()
                            }
                            _ => {
                                return Err(TypeError::new("number or string", lhs)
                                    .with_source_loc(&origin))
                            }
                        },
                        Minus => binary_float(lhs, rhs, |x, y| x - y)?,
                        Multiply => binary_float(lhs, rhs, |x, y| x * y)?,
                        Divide => binary_float(lhs, rhs, |x, y| x / y)?,
                    }
                }
            };

        Ok(val)
    }
}
