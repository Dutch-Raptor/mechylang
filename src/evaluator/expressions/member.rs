use crate::{Environment,  Evaluator, Object, trace};
use crate::evaluator::methods::{MethodError, ObjectMethods};
use crate::parser::expressions::{Expression, ExpressionSpanExt, MemberExpression};
use crate::evaluator::{Result, Error};

impl Evaluator {
    pub(super) fn eval_member_expression(
        &mut self,
        member: &MemberExpression,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_member_expression: {}", member));

        let object = self.eval_expression(&member.object, env)?;

        let property = member.property.value.clone();

        let ident = match *member.object {
            Expression::Identifier(ref ident) => Some(ident.value.clone()),
            _ => None,
        };

        match object.get_method(&property, member.object.span().clone(), member.property.span.clone(), ident) {
            Ok(method) => return Ok(Object::Method(method)),
            Err(MethodError::NotFound) => {}
            Err(MethodError::IterMethodOnIterable(_)) => {
                return Err(Error::IterMethodOnIterable {
                    method_span: member.property.span.clone(),
                    method_name: property.clone(),
                    object_type: object.get_type(),
                    object_span: member.object.span().clone(),
                }.into());
            }
        };

        // try to read property from object
        Err(Error::PropertyNotFound {
            span: member.property.span.clone(),
            property: property.clone(),
            object_type: object.get_type(),
        }.into())
    }
}