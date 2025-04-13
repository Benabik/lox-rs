use std::collections::HashMap;

use log::{debug, info};

use crate::{
    parser::{Block, Declaration, Expression, Function, Statement},
    SourceLoc,
};

#[derive(Clone, Debug)]
pub struct Analyzer<'de> {
    scopes: Vec<HashMap<&'de str, bool>>,
    depths: HashMap<SourceLoc<'de>, usize>,
}

impl<'de> Analyzer<'de> {
    pub fn new_block(block: &Block<'de>) -> miette::Result<Self> {
        let mut ret = Self::default();
        ret.block(block)?;
        assert!(ret.depth() == 1, "Dangling scope!");
        Ok(ret)
    }

    pub fn new_expression(expr: &Expression<'de>) -> miette::Result<Self> {
        let mut ret = Self::default();
        ret.expression(expr)?;
        assert!(ret.depth() == 1, "Dangling scope!");
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

    fn declare_variable(&mut self, name: &'de str) {
        debug!("declaring variable {name} at 0/{}", self.depth());
        self.scopes
            .last_mut()
            .expect("in scope")
            .insert(name, false);
    }

    fn define_variable(&mut self, name: &'de str) {
        debug!("defining variable {name} at 0/{}", self.depth());
        self.scopes.last_mut().expect("in scope").insert(name, true);
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
            }) => {
                self.define_variable(name);
                self.enter_scope();
                for arg in arguments {
                    self.define_variable(arg);
                }
                self.block(body)?;
                self.leave_scope();
                Ok(())
            }
            Declaration::Statement(statement) => self.statement(statement),
            Declaration::Variable(name, expression) => {
                self.declare_variable(name);
                expression
                    .as_ref()
                    .map(|e| self.expression(e))
                    .transpose()?;
                self.define_variable(name);
                Ok(())
            }
        }
    }

    fn statement(&mut self, statement: &Statement<'de>) -> miette::Result<()> {
        match statement {
            Statement::Block(block) => {
                self.enter_scope();
                self.block(block)?;
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
            Statement::Return(expression) => {
                // TODO: Return check
                expression
                    .as_ref()
                    .map(|e| self.expression(e))
                    .unwrap_or(Ok(()))
            }
            Statement::While { condition, body } => {
                self.expression(condition)?;
                self.statement(body)
            }
        }
    }

    fn resolve_variable(&mut self, name: &str, origin: &SourceLoc<'de>) {
        let Some(depth) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(depth, scope)| Some(depth).filter(|_| scope.contains_key(name)))
            .inspect(|depth| {
                info!("resolved {name} at {depth}/{}", self.depth());
            }) else {
                // Failure to resolve falls back to global lookup
                info!("resolved {name} as global");
                return;
            };
        if self.depths.insert(origin.clone(), depth).is_some() {
            let (line, col) = origin.position();
            panic!("Revisiting {name} at {line}:{col}");
        }
    }

    pub fn expression(&mut self, expression: &Expression<'de>) -> miette::Result<()> {
        match expression {
            // Variables and assignment need depth updated
            Expression::Variable { name, origin } => {
                self.resolve_variable(name, origin);
                Ok(())
            },
            Expression::This(_) => todo!(),

            Expression::Assign { name, expr, origin } => {
                self.resolve_variable(name, origin);
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

impl Default for Analyzer<'_> {
    fn default() -> Self {
        let mut ret = Self {
            scopes: Default::default(),
            depths: Default::default(),
        };
        ret.enter_scope();
        ret.define_variable("clock");
        ret
    }
}
