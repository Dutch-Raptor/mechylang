use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    parser::{expressions::Identifier, parser::BlockStatement},
    Environment, EvalConfig, Evaluator, Object,
};

#[derive(Clone)]
pub struct Function {
    pub params: Rc<[Identifier]>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    pub fn call(
        &self,
        args: Vec<Object>,
        env: Option<Environment>,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String> {
        Evaluator::eval_function(Object::Function(self.clone()), args, env, config)
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.body == other.body
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn({})", params)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}
