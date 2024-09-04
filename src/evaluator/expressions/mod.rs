mod prefix_expression;
mod infix_expression;
mod assignment_expression;
mod control_flow;
mod block_expression;
mod identifier;
mod function_calls;
mod array;
mod range;
mod member;
mod r#struct;

use crate::{Environment, Evaluator, Object, trace};
use crate::evaluator::objects::function::Function;
use crate::parser::expressions::Expression;
use crate::evaluator::{Result, };

impl Evaluator {
    pub(super) fn eval_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("Evaluating expression: {}", expression).to_string());
        match expression {
            Expression::IntegerLiteral(lit) => Ok(Object::Integer(lit.value)),
            Expression::FloatLiteral(lit) => Ok(Object::Float(lit.value)),
            Expression::Boolean(boolean) => Ok(boolean.value.into()),
            Expression::Prefix(prefix) => self.eval_prefix_expression(env, prefix),
            Expression::Infix(infix) => self.eval_infix_expression(infix, env),
            Expression::If(if_expr) => self.eval_if_expression(if_expr, env),
            Expression::Block(block) => self.eval_scoped_block_expression(block, env),
            Expression::Identifier(ident) => self.eval_identifier(ident, env),
            Expression::Function(func) => Ok(Object::Function(Function {
                params: func.parameters.clone(),
                span: func.span.clone(),
                body: func.body.clone(),
                env: env.clone(),
            })),
            Expression::Call(call) => self.eval_call_expression(call, env),
            Expression::StringLiteral(lit) => Ok(Object::String(lit.value.clone())),
            Expression::Unit(_) => Ok(Object::Unit),
            Expression::ArrayLiteral(array) => self.eval_array_expression(array, env),
            Expression::Index(index) => self.eval_index_expression(index, env),
            Expression::For(for_expr) => self.eval_for_expression(for_expr, env),
            Expression::While(while_expr) => self.eval_while_expression(while_expr, env),
            Expression::Range(_)
            | Expression::RangeFrom(_)
            | Expression::RangeTo(_)
            | Expression::RangeFull(_) => self.eval_range_expression(expression, env),
            Expression::Member(member) => self.eval_member_expression(member, env),
            Expression::StructLiteral(lit) => self.eval_struct_expression(lit, env),
        }
    }

    pub(super) fn is_truthy(condition: &Object) -> bool {
        match condition {
            Object::Unit => false,
            Object::Boolean(boolean) => *boolean,
            _ => true,
        }
    }

}
