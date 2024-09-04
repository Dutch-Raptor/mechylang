use crate::{Evaluator, Environment, Object, trace, Statement};
use crate::parser::statements::LetStatement;
use crate::evaluator::{Result, };

impl Evaluator {
    pub(super) fn eval_statement(
        &mut self,
        statement: &Statement,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("Evaluating statement: {}", statement).to_string());
        match statement {
            Statement::Expression(stmt) => self.eval_expression(&stmt.expression, env),
            Statement::Return(stmt) => {
                let val = match stmt.return_value {
                    Some(ref expr) => self.eval_expression(expr, env)?,
                    None => Object::Unit,
                };
                Ok(Object::ReturnValue(Box::new(val)))
            }
            Statement::Let(let_statement) => self.eval_let_statement(let_statement, env),
            Statement::Break(break_statement) => match &break_statement.value {
                None => Ok(Object::Break(None)),
                Some(val) => Ok(Object::Break(Some(Box::new(
                    self.eval_expression(val, env)?,
                )))),
            },
            Statement::Continue(_) => Ok(Object::Continue),
            Statement::Function(_) => {
                // Ignore function declarations after initial pass
                Ok(Object::Unit)
            }
        }
    }


    /// Evaluates a let statement.
    ///
    /// Assigns a value to a variable in the current environment.
    ///
    /// Returns `Object::Unit`.
    ///
    /// # Examples
    /// ```
    /// use mechylang::{Evaluator, Environment, Object};
    /// assert_eq!(
    ///    Evaluator::eval("let x = 5", &mut Default::default(), Default::default()).unwrap(),
    ///    Object::Unit
    /// );
    /// assert_eq!(
    ///    Evaluator::eval("let x = 5; x", &mut Default::default(), Default::default()).unwrap(),
    ///    Object::Integer(5)
    /// );
    /// ```
    fn eval_let_statement(
        &mut self,
        let_statement: &LetStatement,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_let_statement: {}", let_statement));
        let val = self.eval_expression(&let_statement.value, env)?;
        env.set(let_statement.name.value.clone(), val);
        Ok(Object::Unit)
    }
}