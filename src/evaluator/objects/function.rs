use std::{
    fmt::{Debug, Display},
    rc::Rc,
};
use std::ops::RangeInclusive;
use crate::{
    parser::{expressions::Identifier},
    Environment, EvalConfig, Evaluator, Object,
};
use crate::parser::expressions::block_expression::BlockExpression;

#[derive(Clone)]
pub struct Function {
    pub params: Rc<[Identifier]>,
    pub body: BlockExpression,
    pub env: Environment,
}

impl Callable for Function {
    fn call(
        &self,
        args: Vec<Object>,
        env: Option<Environment>,
        config: Rc<EvalConfig>,
    ) -> Result<Object, String> {
        Evaluator::eval_function(self.clone(), args, env, config)
    }

    fn args_len(&self) -> RangeInclusive<usize> {
        self.params.len()..=self.params.len()
    }
}

pub trait Callable {
    fn call(&self, args: Vec<Object>, env: Option<Environment>, config: Rc<EvalConfig>) -> Result<Object, String>;
    
    fn args_len(&self) -> RangeInclusive<usize>;
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
