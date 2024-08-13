use crate::{Environment, Error, Evaluator, Object, trace};
use crate::error::ErrorKind;
use crate::evaluator::objects::traits::UnwrapReturnValue;
use crate::evaluator::runtime::builtins::BuiltinFunction;
use crate::parser::expressions::Identifier;

impl Evaluator {
    pub(super) fn eval_identifier(
        &mut self,
        ident: &Identifier,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!(&format!("eval_identifier({})", ident));
        if let Some(object) = env.get(ident.value.clone()) {
            // If the value is a return value, unwrap it
            Ok(object.unwrap_return_value())
        } else if let Some(object) = self.globals.get(&ident.value) {
            Ok(object.clone())
        } else if let Ok(builtin) = BuiltinFunction::try_from(ident) {
            Ok(builtin.into())
        } else {
            Err(self.error(
                ident.span.clone(),
                format!("Identifier not found: {}", ident.value).as_str(),
                ErrorKind::IdentifierNotFound,
            ))
        }
    }
}