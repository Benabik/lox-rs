use std::collections::HashMap;

use log::{debug, info};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    parser::{Block, Declaration, Expression, Function, Statement},
    SourceLoc,
};

#[derive(Diagnostic, Debug, Error)]
#[error("Already a variable '{name}' with this name in this scope")]
pub struct RedeclaredVariableError {
    name: String,

    #[label("here")]
    span: SourceSpan,

    #[source_code]
    src: String,
}

impl RedeclaredVariableError {
    fn new<T: ToString>(name: T, origin: &SourceLoc) -> Self {
        Self {
            name: name.to_string(),
            span: origin.into(),
            src: origin.source.to_string(),
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Can't read local variable '{name}' in its own initializer")]
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

#[derive(Clone, Debug, Default)]
pub struct Analyzer<'de> {
    scopes: Vec<HashMap<&'de str, bool>>,
    depths: HashMap<SourceLoc<'de>, usize>,
}

impl<'de> Analyzer<'de> {
    pub fn new_block(block: &Block<'de>) -> miette::Result<Self> {
        let mut ret = Self::default();
        ret.block(block)?;
        assert!(ret.depth() == 0, "Dangling scope!");
        Ok(ret)
    }

    pub fn new_expression(expr: &Expression<'de>) -> miette::Result<Self> {
        let mut ret = Self::default();
        ret.expression(expr)?;
        assert!(ret.depth() == 0, "Dangling scope!");
        Ok(ret)
    }

    pub fn depth_for(&self, origin: &SourceLoc<'de>) -> Option<usize> {
        self.depths.get(origin).copied()
    }

    fn enter_scope(&mut self) {
        debug!("entering scope");
        self.scopes.push(Default::default())
    }

    fn depth(&self) -> usize {
        self.scopes.len()
    }

    fn leave_scope(&mut self) {
        debug!("leaving scope depth {}", self.depth());
        self.scopes.pop();
    }

    fn setup_variable(
        &mut self,
        name: &'de str,
        origin: &SourceLoc<'de>,
        defined: bool,
    ) -> miette::Result<()> {
        let Some(scope) = self.scopes.last_mut() else {
            // Global
            return Ok(());
        };

        // Compare what we're doing to the current state
        match (defined, scope.insert(name, defined)) {
            (_, Some(true)) => Err(RedeclaredVariableError::new(name, origin).into()),
            (false, Some(false)) => panic!("Redeclaring undefined variable?"),
            (true, Some(false)) => Ok(()), // Defining a declared
            (_, None) => Ok(()),           // New variable
        }
        // }).ok_or_else(||
    }

    fn declare_variable(
        &mut self,
        name: &'de str,
        origin: &SourceLoc<'de>,
    ) -> Result<(), miette::Error> {
        debug!("declaring variable {name} at 0/{}", self.depth());
        self.setup_variable(name, origin, false)
    }

    fn define_variable(
        &mut self,
        name: &'de str,
        origin: &SourceLoc<'de>,
    ) -> Result<(), miette::Error> {
        debug!("defining variable {name} at 0/{}", self.depth());
        self.setup_variable(name, origin, true)
    }

    pub fn block(&mut self, block: &Block<'de>) -> miette::Result<()> {
        block.0.iter().try_for_each(|s| self.declaration(s))
    }

    fn declaration(&mut self, decl: &Declaration<'de>) -> miette::Result<()> {
        match decl {
            Declaration::Class { .. } => todo!(),
            Declaration::Function(Function {
                name,
                arguments,
                body,
                origin,
            }) => {
                self.define_variable(name, origin)?;
                self.enter_scope();
                for arg in arguments {
                    self.define_variable(arg, origin)?;
                }
                self.block(body)?;
                self.leave_scope();
                Ok(())
            }
            Declaration::Statement(statement) => self.statement(statement),
            Declaration::Variable { name, init, origin } => {
                self.declare_variable(name, origin)?;
                init.as_ref().map(|e| self.expression(e)).transpose()?;
                self.define_variable(name, origin)?;
                Ok(())
            }
        }
    }

    fn statement(&mut self, statement: &Statement<'de>) -> miette::Result<()> {
        match statement {
            Statement::Block { body, .. } => {
                self.enter_scope();
                self.block(body)?;
                self.leave_scope();
                Ok(())
            }
            Statement::Expression(expression) => self.expression(expression),
            Statement::If {
                condition,
                then,
                other,
            } => {
                self.expression(condition)?;
                self.statement(then)?;
                other.as_ref().map(|s| self.statement(s)).unwrap_or(Ok(()))
            }
            Statement::Print(expression) => self.expression(expression),
            Statement::Return { expression, origin } => {
                // TODO: Return check
                expression
                    .as_ref()
                    .map(|e| self.expression(e))
                    .unwrap_or(Ok(()))
            }
            Statement::While {
                condition, body, ..
            } => {
                self.expression(condition)?;
                self.statement(body)
            }
        }
    }

    fn resolve_variable(&mut self, name: &str, origin: &SourceLoc<'de>) -> miette::Result<()> {
        // Iterate back through scopes, returning the first value and its depth
        match self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(depth, scope)| scope.get(name).map(|init| (depth, init)))
        {
            Some((depth, true)) => {
                info!("resolved {name} at {depth}/{}", self.depth());
                if self.depths.insert(origin.clone(), depth).is_some() {
                    let (line, col) = origin.position();
                    panic!("Revisiting {name} at {line}:{col}");
                }
                Ok(())
            }
            Some((_, false)) => Err(UndefinedVariableError::new(name, origin).into()),
            None => {
                info!("resolved {name} as global");
                Ok(())
            }
        }
    }

    pub fn expression(&mut self, expression: &Expression<'de>) -> miette::Result<()> {
        match expression {
            // Variables and assignment need depth updated
            Expression::Variable { name, origin } => {
                self.resolve_variable(name, origin)?;
                Ok(())
            }
            Expression::This(_) => todo!(),

            Expression::Assign { name, expr, origin } => {
                self.resolve_variable(name, origin)?;
                self.expression(expr)
            }
            Expression::AssignProp { .. } => todo!(),

            // Literals require no anaylsis
            Expression::Literal { .. } => Ok(()),
            // Everything else just recurses
            Expression::Unary { expr, .. } => self.expression(expr),
            Expression::Binary { lhs, rhs, .. } => {
                self.expression(lhs)?;
                self.expression(rhs)
            }
            Expression::Grouping { expr, .. } => self.expression(expr),
            Expression::Call {
                callee, arguments, ..
            } => {
                self.expression(callee)?;
                arguments.0.iter().try_for_each(|e| self.expression(e))
            }
            Expression::Property { .. } => todo!(),
        }
    }
}
