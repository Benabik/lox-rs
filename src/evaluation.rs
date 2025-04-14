use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::SystemTime;

use crate::parser::{Block, Declaration, Expression, Function, LiteralValue, Statement};
use crate::{analyzer::Analyzer, parser, SourceLoc, WithSourceLoc};
use derive_more::{Display, From};
use log::{debug, error};
use miette::{Diagnostic, IntoDiagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug, Display, From, PartialEq)]
#[display("<fn {name}>")]
pub struct Closure<'de> {
    pub name: &'de str,
    pub arguments: Vec<&'de str>,
    pub body: Block<'de>,
    pub environment: Environment<'de>,
}

impl<'de> Closure<'de> {
    pub fn bind(&self, object: impl Into<Pointer<'de>>) -> Self {
        let mut environment = self.environment.push();
        environment.define("this", object);
        Self {
            environment,
            ..self.clone()
        }
    }
}

#[derive(Clone, Debug, Display, PartialEq)]
#[display("{name}")]
pub struct Class<'de> {
    pub name: &'de str,
    pub methods: HashMap<&'de str, Closure<'de>>,
}

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
        body: fn(&[Pointer<'de>]) -> miette::Result<Pointer<'de>>,
    },
    Class(Rc<Class<'de>>),
    Closure(Closure<'de>),
    Number(f64),
    #[display("{} instance", class.name)]
    Object {
        class: Rc<Class<'de>>,
        properties: PointerMap<'de>,
    },
    String(String),
}

impl<'de> From<Class<'de>> for Value<'de> {
    fn from(value: Class<'de>) -> Self {
        Self::Class(Rc::new(value))
    }
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

#[derive(Clone, Default, Debug, PartialEq)]
pub struct Pointer<'de>(Rc<RefCell<Value<'de>>>);

impl<'de> Pointer<'de> {
    pub fn borrow(&self) -> Ref<'_, Value<'de>> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, Value<'de>> {
        self.0.borrow_mut()
    }

    pub fn try_into_str(&self) -> Result<Ref<'_, str>, TypeError> {
        let value = self.borrow();
        Ref::filter_map(value, |value| match value {
            Value::String(s) => Some(&s[..]),
            _ => None,
        })
        .map_err(|value| TypeError::new("string", value))
    }
}

impl Display for Pointer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.borrow().fmt(f)
    }
}

impl<'de, T: Into<Value<'de>>> From<T> for Pointer<'de> {
    fn from(value: T) -> Self {
        Self(Rc::new(RefCell::new(value.into())))
    }
}

impl From<Pointer<'_>> for bool {
    fn from(value: Pointer<'_>) -> Self {
        Self::from(&value)
    }
}

impl From<&Pointer<'_>> for bool {
    fn from(value: &Pointer) -> Self {
        // Lox uses truthyness, not strictly typed booleans
        match &*value.borrow() {
            Value::Nil => false,
            Value::Builtin { .. } => true,
            Value::Boolean(value) => *value,
            Value::Class(_) => true,
            Value::Closure { .. } => true,
            Value::Number(_) => true,
            Value::Object { .. } => true,
            Value::String(_) => true,
        }
    }
}

impl TryFrom<&Pointer<'_>> for f64 {
    type Error = TypeError;

    fn try_from(value: &Pointer) -> Result<Self, Self::Error> {
        let value = value.borrow();
        match &*value {
            Value::Number(value) => Ok(*value),
            _ => Err(TypeError::new("number", value)),
        }
    }
}

impl TryFrom<&Pointer<'_>> for String {
    type Error = TypeError;

    fn try_from(value: &Pointer) -> Result<Self, Self::Error> {
        let value = value.borrow();
        match &*value {
            Value::String(value) => Ok(value.clone()),
            _ => Err(TypeError::new("string", value)),
        }
    }
}

type PointerMap<'de> = HashMap<&'de str, Pointer<'de>>;

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
    pub fn new(expected: &'static str, value: impl ToString) -> Self {
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
#[error("Undefined variable '{name}'.")]
pub struct UndefinedVariableError {
    name: String,

    #[label("here")]
    span: SourceSpan,

    #[source_code]
    src: String,
}

impl UndefinedVariableError {
    fn new<T: ToString>(name: T, origin: &SourceLoc) -> Self {
        Self {
            name: name.to_string(),
            span: origin.into(),
            src: origin.source.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Undefined property '{name}'.")]
pub struct UndefinedPropertyError {
    name: String,

    #[label("here")]
    span: SourceSpan,

    #[source_code]
    src: String,
}

impl UndefinedPropertyError {
    fn new<T: ToString>(name: T, origin: &SourceLoc) -> Self {
        Self {
            name: name.to_string(),
            span: origin.into(),
            src: origin.source.to_string(),
        }
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
    values: PointerMap<'de>,
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

    fn ancestor(&self, depth: usize) -> Environment<'de> {
        std::iter::repeat(())
            .take(depth)
            .try_fold(self.clone(), |env, ()| env.0.borrow().parent.clone())
            .expect("Ran out of scopes")
    }

    fn get(&self, name: &str, origin: &SourceLoc) -> miette::Result<Pointer<'de>> {
        let frame = self.0.borrow();
        frame
            .values
            .get(name)
            .cloned()
            .ok_or_else(|| UndefinedVariableError::new(name, origin).into())
    }

    fn assign(
        &mut self,
        name: &str,
        value: Pointer<'de>,
        origin: &SourceLoc,
    ) -> miette::Result<Pointer<'de>> {
        let mut frame = self.0.borrow_mut();
        frame
            .values
            .get_mut(name)
            .map(|v| {
                *v = value.clone();
                value
            })
            .ok_or_else(|| UndefinedVariableError::new(name, origin).into())
    }

    fn define(&mut self, name: &'de str, value: impl Into<Pointer<'de>>) {
        let value = value.into();
        debug!("Defining {name} at 0/{}", self.depth());
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
    globals: Environment<'de>,
    scope: Environment<'de>,
    analysis: Analyzer<'de>,
}

impl<'de> Interpreter<'de> {
    pub fn new(analysis: Analyzer<'de>) -> Self {
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
            scope: globals.clone(),
            globals,
            analysis,
        }
    }

    fn function(&mut self, function: &Function<'de>) -> Closure<'de> {
        let Function {
            name,
            arguments,
            body,
            ..
        } = function;
        Closure {
            name,
            arguments: arguments.clone(),
            body: body.clone(),
            environment: self.scope.clone(),
        }
    }

    pub fn block(&mut self, prog: &Block<'de>) -> miette::Result<Option<Pointer<'de>>> {
        for d in &prog.0 {
            match d {
                Declaration::Class { name, methods, .. } => {
                    let methods = methods.iter().map(|f| (f.name, self.function(f))).collect();
                    self.scope.define(name, Class { name, methods });
                }
                Declaration::Function(f) => {
                    let f = self.function(f);
                    self.scope.define(f.name, f);
                }

                Declaration::Variable { name, init, .. } => {
                    let value = if let Some(expr) = init {
                        self.expression(expr)?
                    } else {
                        Default::default()
                    };
                    self.scope.define(name, value);
                }

                Declaration::Statement(s) => {
                    // Statement was a return
                    if let Some(value) = self.statement(s)? {
                        return Ok(Some(value));
                    }
                }
            }
        }
        Ok(None)
    }

    fn statement(&mut self, stmt: &Statement<'de>) -> miette::Result<Option<Pointer<'de>>> {
        use parser::Statement::*;
        let ret = match stmt {
            Block(block) => {
                debug!("Entering block scope");
                self.scope = self.scope.push();
                let ret = self.block(block)?;
                debug!("Leaving block scope");
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
            Return(None) => Some(Default::default()),
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

    fn get(&self, name: &'de str, origin: &SourceLoc<'de>) -> miette::Result<Pointer<'de>> {
        let environment = if let Some(depth) = self.analysis.depth_for(origin) {
            debug!("Getting {name} {depth}/{}", self.scope.depth());
            &self.scope.ancestor(depth)
        } else {
            debug!("Getting global {name}");
            &self.globals
        };
        environment.get(name, origin)
    }

    fn assign(
        &mut self,
        name: &'de str,
        value: impl Into<Pointer<'de>>,
        origin: &SourceLoc<'de>,
    ) -> miette::Result<Pointer<'de>> {
        let value = value.into();
        let environment = if let Some(depth) = self.analysis.depth_for(origin) {
            debug!("Getting {name} {depth}/{}", self.scope.depth());
            &mut self.scope.ancestor(depth)
        } else {
            debug!("Getting global {name}");
            &mut self.globals
        };
        environment.assign(name, value, origin)
    }

    pub fn expression(&mut self, expr: &Expression<'de>) -> miette::Result<Pointer<'de>> {
        let to_float =
            |val: Pointer, origin: &SourceLoc| f64::try_from(&val).with_source_loc(origin);

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
                self.assign(name, value, origin)?
            }
            Expression::AssignProp {
                object,
                name,
                expr,
                origin,
            } => {
                let object = self.expression(object)?;
                let Value::Object { properties, .. } = &mut *object.borrow_mut() else {
                    return Err(TypeError::new("object", object).with_source_loc(origin));
                };
                let value = self.expression(expr)?;
                properties.insert(name, value.clone());
                value
            }
            Expression::Call {
                callee,
                arguments,
                origin,
            } => {
                let callee = self.expression(callee)?;

                let arity = match &*callee.borrow() {
                    Value::Builtin { arity, .. } => *arity,
                    Value::Class { .. } => 0,
                    Value::Closure(Closure { arguments, .. }) => arguments.len(),
                    _ => {
                        return Err(TypeError::new("function", &callee).with_source_loc(origin));
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

                let callee = callee.borrow();
                match &*callee {
                    Value::Builtin { body, .. } => {
                        return body(&arguments);
                    }
                    Value::Class(class) => Value::Object {
                        class: class.clone(),
                        properties: Default::default(),
                    }
                    .into(),
                    Value::Closure(Closure {
                        name,
                        body,
                        arguments: names,
                        environment: parent,
                    }) => {
                        let outer = self.scope.clone();
                        debug!("Entering {name} scope");
                        self.scope = parent.push();
                        for (name, value) in names.iter().zip(arguments) {
                            self.scope.define(name, value);
                        }
                        let ret = self.block(body)?;
                        debug!("Leaving {name} scope");
                        self.scope = outer;
                        ret.unwrap_or_default()
                    }
                    _ => unreachable!("type matched above"),
                }
            }

            Expression::Property {
                object,
                name,
                origin,
            } => {
                let object = self.expression(object)?;
                let Value::Object {
                    class, properties, ..
                } = &*object.borrow()
                else {
                    return Err(TypeError::new("object", object).with_source_loc(origin));
                };
                properties
                    .get(name)
                    .cloned()
                    .or_else(|| {
                        class
                            .methods
                            .get(name)
                            .map(|c| c.bind(object.clone()).into())
                    })
                    .ok_or_else(|| UndefinedPropertyError::new(name, origin))?
            }

            Expression::Binary {
                op,
                lhs,
                rhs,
                origin,
            } => {
                let lhs = self.expression(lhs)?;

                // Evaluate logical ops before RHS for short-circuiting
                use parser::BinaryOp::*;
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
                    Ok::<Pointer, miette::Report>(f(lhs, rhs).into())
                };

                match op {
                    Or | And => unreachable!("matched above"),
                    Equal => Pointer::from(lhs == rhs),
                    NotEqual => Pointer::from(lhs != rhs),
                    Less | LessEqual | Greater | GreaterEqual => match &*lhs.borrow() {
                        Value::Number(lhs) => {
                            let lhs = *lhs;
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
                            let lhs = &lhs[..];
                            let rhs = &*rhs.try_into_str().with_source_loc(origin)?;
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
                                TypeError::new("number or string", &lhs).with_source_loc(origin)
                            )
                        }
                    },
                    Plus => match &*lhs.borrow() {
                        Value::Number(lhs) => (lhs + to_float(rhs, origin)?).into(),
                        Value::String(lhs) => {
                            let mut lhs = lhs.clone();
                            let rhs = &*rhs.try_into_str().with_source_loc(origin)?;
                            lhs += rhs;
                            lhs.into()
                        }
                        _ => {
                            return Err(
                                TypeError::new("number or string", &lhs).with_source_loc(origin)
                            )
                        }
                    },
                    Minus => binary_float(lhs, rhs, |x, y| x - y)?,
                    Multiply => binary_float(lhs, rhs, |x, y| x * y)?,
                    Divide => binary_float(lhs, rhs, |x, y| x / y)?,
                }
            }

            Expression::Variable { name, origin } => self.get(name, origin)?,
            Expression::This(origin) => self.get("this", origin)?,
        };

        Ok(val)
    }
}
