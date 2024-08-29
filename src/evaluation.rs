use crate::{parser, WithSourceLoc};
use crate::parser::{Expression, LiteralValue};
use derive_more::{Display, From};

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

pub fn evaluate(expr: Expression) -> miette::Result<Value> {
    let val = match expr {
        Expression::Literal(parser::Literal { value, .. }) => value.into(),
        _ => return Err(miette::diagnostic! {
            "Expression type NYI"
        }
        .with_source_loc(expr.origin())),
    };

    Ok(val)
}
