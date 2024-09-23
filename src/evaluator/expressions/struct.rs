use std::collections::HashMap;
use crate::{Environment,  Evaluator, Object, trace};
use crate::parser::expressions::StructLiteral;
use crate::evaluator::Result;

impl Evaluator {
    pub(super) fn eval_struct_expression(
        &mut self,
        lit: &StructLiteral,
        env: &mut Environment,
    ) -> Result<Object> {
        let _trace = trace!("Eval struct expression");
        let mut map = HashMap::new();

        for (key, expression) in &lit.entries {
            let object = self.eval_expression(expression, env)?;
            map.insert(key.to_string(), object);
        }

        Ok(Object::Struct(map))
    }
}