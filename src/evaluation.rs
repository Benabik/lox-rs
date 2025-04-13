use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;

use crate::parser::{Block, Expression, LiteralValue, Statement};
use crate::{analyzer::Analyzer, parser, SourceLoc, WithSourceLoc};
use derive_more::{Display, From};
use miette::{Diagnostic, IntoDiagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Default, Debug, Display, From, PartialEq)]
pub enum Value<'de> {
    #[display("nil")]
    #[default]
    Nil,
    Boolean(bool),
    #[display("<fn {name}>")]
    Builtin {
        name: &'static str,
        arity: usize,
        body: fn(&[Value<'de>]) -> miette::Result<Value<'de>>,
    },
    #[display("<fn {name}>")]
    Closure {
        name: &'de str,
        arguments: Vec<&'de str>,
        body: Block<'de>,
        environment: Environment<'de>,
    },
    Number(f64),
    String(String),
}

impl<'a> From<&LiteralValue<'a>> for Value<'_> {
    fn from(value: &LiteralValue<'a>) -> Self {
        match value {
            LiteralValue::Number(val) => Value::Number(*val),
            LiteralValue::String(val) => Value::String(val.to_string()),
            LiteralValue::Boolean(val) => Value::Boolean(*val),
            LiteralValue::Nil => Value::Nil,
        }
    }
}

impl<'a> From<LiteralValue<'a>> for Value<'_> {
    fn from(value: LiteralValue<'a>) -> Self {
        Value::from(&value)
    }
}

impl From<&Value<'_>> for bool {
    fn from(value: &Value) -> Self {
        // Lox uses truthyness, not strictly typed booleans
        match value {
            Value::Nil => false,
            Value::Builtin { .. } => true,
            Value::Boolean(value) => *value,
            Value::Closure { .. } => true,
            Value::Number(_) => true,
            Value::String(_) => true,
        }
    }
}

impl From<Value<'_>> for bool {
    fn from(value: Value) -> Self {
        bool::from(&value)
    }
}

impl TryFrom<Value<'_>> for f64 {
    type Error = TypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(value) => Ok(value),
            _ => Err(TypeError::new("number", value)),
        }
    }
}

impl TryFrom<Value<'_>> for String {
    type Error = TypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(value),
            _ => Err(TypeError::new("string", value)),
        }
    }
}

impl<'a> TryFrom<&'a Value<'_>> for &'a str {
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
    value: String, // Avoiding threading the parser lifetime into errors

    #[label("here")]
    span: Option<SourceSpan>,

    #[source_code]
    src: Option<String>,
}

impl TypeError {
    pub fn new(expected: &'static str, value: Value) -> Self {
        TypeError {
            expected,
            value: value.to_string(),
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

#[derive(Diagnostic, Debug, Error)]
#[error("Wrong number of arguments, expected {expected}, got {got}")]
pub struct BadArityError {
    expected: usize,
    got: usize,

    #[label("here")]
    span: Option<SourceSpan>,

    #[source_code]
    src: Option<String>,
}

impl BadArityError {
    pub fn new(expected: usize, got: usize) -> Self {
        Self {
            expected,
            got,
            span: None,
            src: None,
        }
    }
}

impl WithSourceLoc for BadArityError {
    type Wrapped = miette::Error;

    fn with_source_loc(mut self, loc: &SourceLoc) -> Self::Wrapped {
        self.src = Some(loc.source.to_string());
        self.span = Some(loc.into());
        self.into()
    }
}

#[derive(Clone, Default, Debug)]
struct Frame<'de> {
    parent: Option<Environment<'de>>,
    values: HashMap<&'de str, Value<'de>>,
}

#[derive(Clone, Default, Debug)]
pub struct Environment<'de>(Rc<RefCell<Frame<'de>>>);

impl<'de> Environment<'de> {
    fn new(parent: Environment<'de>) -> Self {
        Self(Rc::new(RefCell::new(Frame {
            parent: Some(parent),
            values: Default::default(),
        })))
    }

    fn depth(&self) -> usize {
        self.0
            .borrow()
            .parent
            .as_ref()
            .map(|parent| parent.depth() + 1)
            .unwrap_or(1)
    }

    fn push(&self) -> Self {
        Self::new(self.clone())
    }

    fn pop(&self) -> Option<Self> {
        (*self.0).borrow().parent.clone()
    }

    fn missing_variable(&self, name: &str, depth: usize, origin: &SourceLoc) -> ! {
        let (line, col) = origin.position();
        panic!(
            "Missing variable {name} {depth}/{} at {line}:{col}",
            self.depth()
        );
    }

    fn get_impl(&mut self, name: &str, depth: usize) -> Option<Value<'de>> {
        let (values, mut parent) = RefMut::map_split(self.0.borrow_mut(), |frame| {
            (&mut frame.values, &mut frame.parent)
        });
        if depth == 0 {
            values.get(name).cloned()
        } else {
            parent
                .as_mut()
                .and_then(|parent| parent.get_impl(name, depth - 1))
        }
    }

    fn get(&mut self, name: &str, depth: usize, origin: &SourceLoc) -> miette::Result<Value<'de>> {
        self.get_impl(name, depth)
            .ok_or_else(|| self.missing_variable(name, depth, origin))
    }

    fn assign_impl(&mut self, name: &str, depth: usize, value: Value<'de>) -> Option<Value<'de>> {
        let (mut values, mut parent) = RefMut::map_split(self.0.borrow_mut(), |frame| {
            (&mut frame.values, &mut frame.parent)
        });

        if depth == 0 {
            values.get_mut(name).map(|v| {
                *v = value.clone();
                value
            })
        } else {
            parent
                .as_mut()
                .and_then(|parent| parent.assign_impl(name, depth - 1, value))
        }
    }

    fn assign(
        &mut self,
        name: &str,
        depth: usize,
        value: Value<'de>,
        origin: &SourceLoc,
    ) -> miette::Result<Value<'de>> {
        self.assign_impl(name, depth, value)
            .ok_or_else(|| self.missing_variable(name, depth, origin))
    }

    fn define(&mut self, name: &'de str, value: Value<'de>) {
        self.0.borrow_mut().values.insert(name, value);
    }
}

impl PartialEq for Environment<'_> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Clone, Debug)]
pub struct Interpreter<'de> {
    scope: Environment<'de>,
    analysis: Option<Analyzer<'de>>,
}

impl<'de> Interpreter<'de> {
    fn depth_for(&self, name: &'de str, origin: &SourceLoc<'de>) -> usize {
        self.analysis
            .as_ref()
            .expect("anaylsis done")
            .depth_for(origin)
            .unwrap_or_else(|| {
                let (line, col) = origin.position();
                panic!("Unanalyzed variable {name} at {line}:{col}");
            })
    }

    pub fn run_block(&mut self, prog: &Block<'de>) -> miette::Result<Option<Value<'de>>> {
        self.analysis.replace(Analyzer::new_block(prog)?);
        self.block(prog)
    }

    fn block(&mut self, prog: &Block<'de>) -> miette::Result<Option<Value<'de>>> {
        for d in &prog.0 {
            use parser::Declaration::*;
            match d {
                Function {
                    name,
                    arguments,
                    body,
                } => {
                    self.scope.define(
                        name,
                        Value::Closure {
                            name,
                            arguments: arguments.clone(),
                            body: body.clone(),
                            environment: self.scope.clone(),
                        },
                    );
                }

                Variable(name, expr) => {
                    let value = if let Some(expr) = expr {
                        self.expression(expr)?
                    } else {
                        Value::Nil
                    };
                    self.scope.define(name, value);
                }

                Statement(s) => {
                    // Statement was a return
                    if let Some(value) = self.statement(s)? {
                        return Ok(Some(value));
                    }
                }
            }
        }
        Ok(None)
    }

    fn statement(&mut self, stmt: &Statement<'de>) -> miette::Result<Option<Value<'de>>> {
        use parser::Statement::*;
        let ret = match stmt {
            Block(block) => {
                self.scope = self.scope.push();
                let ret = self.block(block)?;
                self.scope = self.scope.pop().expect("exited top scope");
                ret
            }
            Expression(e) => {
                self.expression(e)?;
                None
            }
            If {
                condition,
                then,
                other,
            } => {
                let s = if self.expression(condition)?.into() {
                    Some(then)
                } else {
                    other.as_ref()
                };
                s.map(|s| self.statement(s)).transpose()?.flatten()
            }
            Print(e) => {
                println!("{}", self.expression(e)?);
                None
            }
            Return(Some(e)) => Some(self.expression(e)?),
            Return(None) => Some(Value::Nil),
            While { condition, body } => {
                while self.expression(condition)?.into() {
                    if let Some(value) = self.statement(body)? {
                        return Ok(Some(value));
                    }
                }
                None
            }
        };
        Ok(ret)
    }

    pub fn run_expression(&mut self, expr: &Expression<'de>) -> miette::Result<Value<'de>> {
        self.analysis.replace(Analyzer::new_expression(expr)?);
        self.expression(expr)
    }

    fn expression(&mut self, expr: &Expression<'de>) -> miette::Result<Value<'de>> {
        let to_float = |val: Value, origin: &SourceLoc| f64::try_from(val).with_source_loc(origin);
        let to_string =
            |val: Value, origin: &SourceLoc| String::try_from(val).with_source_loc(origin);

        let val = match expr {
            Expression::Literal { value, .. } => value.into(),
            Expression::Grouping { expr, .. } => return self.expression(expr),
            Expression::Unary { op, expr, origin } => {
                let value = self.expression(expr)?;
                match op {
                    parser::UnaryOp::Negate => (-to_float(value, origin)?).into(),
                    parser::UnaryOp::Not => (!bool::from(value)).into(),
                }
            }
            Expression::Assign { name, expr, origin } => {
                let value = self.expression(expr)?;
                let depth = self.depth_for(name, origin);
                self.scope.assign(name, depth, value, origin)?
            }
            Expression::Call {
                callee,
                arguments,
                origin,
            } => {
                let callee = self.expression(callee)?;

                let arity = match &callee {
                    Value::Builtin { arity, .. } => *arity,
                    Value::Closure { arguments, .. } => arguments.len(),
                    _ => {
                        return Err(TypeError::new("function", callee).with_source_loc(origin));
                    }
                };

                let arguments = &arguments.0;
                if arity != arguments.len() {
                    return Err(BadArityError::new(arity, arguments.len()).with_source_loc(origin));
                }

                let arguments = arguments
                    .iter()
                    .map(|a| self.expression(a))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee {
                    Value::Builtin { body, .. } => {
                        return body(&arguments);
                    }
                    Value::Closure {
                        body,
                        arguments: names,
                        environment: parent,
                        ..
                    } => {
                        let outer = self.scope.clone();
                        self.scope = parent.push();
                        for (name, value) in names.iter().zip(arguments) {
                            self.scope.define(name, value);
                        }
                        let ret = self.block(&body)?;
                        self.scope = outer;
                        ret.unwrap_or_default()
                    }
                    _ => unreachable!("type matched above"),
                }
            }
            Expression::Binary {
                op,
                lhs,
                rhs,
                origin,
            } => {
                let lhs = self.expression(lhs)?;

                // Evaluate logical ops before RHS for short-circuiting
                if matches!(op, Or | And) {
                    let truth = bool::from(&lhs);
                    return Ok(match (truth, op) {
                        (true, Or) => lhs,
                        (false, And) => lhs,
                        _ => self.expression(rhs)?,
                    });
                }

                let rhs = self.expression(rhs)?;

                let binary_float = |lhs, rhs, f: fn(f64, f64) -> f64| {
                    let lhs = to_float(lhs, origin)?;
                    let rhs = to_float(rhs, origin)?;
                    Ok::<Value, miette::Report>(f(lhs, rhs).into())
                };

                use parser::BinaryOp::*;
                match op {
                    Or | And => unreachable!("matched above"),
                    Equal => Value::from(lhs == rhs),
                    NotEqual => Value::from(lhs != rhs),
                    Less | LessEqual | Greater | GreaterEqual => match lhs {
                        Value::Number(lhs) => {
                            let rhs = to_float(rhs, origin)?;
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
                            let rhs = to_string(rhs, origin)?;
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
                            return Err(
                                TypeError::new("number or string", lhs).with_source_loc(origin)
                            )
                        }
                    },
                    Plus => match lhs {
                        Value::Number(lhs) => (lhs + to_float(rhs, origin)?).into(),
                        Value::String(mut lhs) => {
                            lhs += &to_string(rhs, origin)?;
                            lhs.into()
                        }
                        _ => {
                            return Err(
                                TypeError::new("number or string", lhs).with_source_loc(origin)
                            )
                        }
                    },
                    Minus => binary_float(lhs, rhs, |x, y| x - y)?,
                    Multiply => binary_float(lhs, rhs, |x, y| x * y)?,
                    Divide => binary_float(lhs, rhs, |x, y| x / y)?,
                }
            }

            Expression::Variable { name, origin } => {
                self.scope.get(name, self.depth_for(name, origin), origin)?
            }
        };

        Ok(val)
    }
}

impl Default for Interpreter<'_> {
    fn default() -> Self {
        let mut globals = Environment::default();

        globals.define(
            "clock",
            Value::Builtin {
                name: "clock",
                arity: 0,
                body: |_| {
                    SystemTime::now()
                        .duration_since(SystemTime::UNIX_EPOCH)
                        .map(|d| d.as_secs_f64().into())
                        .into_diagnostic()
                },
            },
        );

        Interpreter {
            scope: globals,
            analysis: None,
        }
    }
}
