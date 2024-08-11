use crate::{Environment, Error, Evaluator, Object, trace};
use crate::error::ErrorKind;
use crate::evaluator::methods::{MethodError, ObjectMethods};
use crate::parser::expressions::{Expression, MemberExpression};

impl Evaluator {
    pub(super) fn eval_member_expression(
        &mut self,
        member: &MemberExpression,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_member_expression: {}", member));

        let object = self.eval_expression(&member.object, env)?;

        let property = member.property.value.clone();

        let ident = match *member.object {
            Expression::Identifier(ref ident) => Some(ident.value.clone()),
            _ => None,
        };

        match object.get_method(&property, ident) {
            Ok(method) => return Ok(Object::Method(method)),
            Err(MethodError::NotFound) => {}
            Err(err @ MethodError::IterMethodOnIterable(_)) => {
                return Err(self.error(
                    Some(&member.property.token),
                    &err.to_string(),
                    ErrorKind::TypeError,
                ))
            }
        };

        // try to read property from object
        Err(self.error(
            Some(&member.property.token),
            &format!(
                "Property or method '{}' not found on object: {:?}",
                property, object
            )
                .to_string(),
            ErrorKind::PropertyNotFound,
        ))
    }
}