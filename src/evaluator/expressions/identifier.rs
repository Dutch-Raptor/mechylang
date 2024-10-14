use strsim::jaro_winkler;
use crate::{Environment, Evaluator, Object, trace};
use crate::evaluator::{Result, Error};
use crate::evaluator::objects::traits::UnwrapReturnValue;
use crate::evaluator::runtime::builtins::BuiltinFunction;
use crate::parser::expressions::Identifier;

impl Evaluator {
    pub(super) fn eval_identifier(
        &mut self,
        ident: &Identifier,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!(&format!("eval_identifier({})", ident));
        if let Some(object) = env.get(ident.value.clone()) {
            // If the value is a return value, unwrap it
            Ok(object.unwrap_return_value())
        } else if let Some(object) = self.globals.get(&ident.value) {
            Ok(object.clone())
        } else if let Ok(builtin) = BuiltinFunction::try_from(ident) {
            Ok(builtin.into())
        } else {
            Err(Self::identifier_not_found_error(ident, env).into())
        }
    }


    pub(in crate::evaluator) fn identifier_not_found_error(ident: &Identifier, env: &Environment) -> Error {
        let all_vars = env.get_all_keys();
        let similar = all_vars.into_iter()
            .filter_map(|key| {
                let sim_score = jaro_winkler(&key, &ident.value);
                if sim_score > 0.8 {
                    Some((key, sim_score))
                } else {
                    None
                }
            })
            .max_by(|a, b| {
                a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal)
            }).map(|(s, _)| s);

        Error::IdentifierNotFound {
            span: ident.span.clone(),
            identifier: ident.value.clone(),
            similar,
        }
    }
}