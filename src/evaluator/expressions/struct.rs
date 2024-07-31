use std::collections::HashMap;
use crate::{Environment, Error, Evaluator, Object, trace};
use crate::parser::expressions::StructLiteral;

impl Evaluator {
    pub(super) fn eval_struct_expression(
        &mut self,
        lit: &StructLiteral,
        env: &mut Environment,
    ) -> Result<Object, Error> {
        let _trace = trace!("Eval struct expression");
        let mut map = HashMap::new();

        for (key, expression) in &lit.entries {
            let object = self.eval_expression(expression, env)?;
            let object_id = env.store_object(object.clone());
            map.insert(key.to_string(), object_id);
        }

        Ok(Object::Struct(map))
    }
}