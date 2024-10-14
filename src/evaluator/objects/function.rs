use std::{
    fmt::{Debug, Display},
    rc::Rc,
};
use std::ops::RangeInclusive;
use crate::{parser::{expressions::{BlockExpression, Identifier}}, Environment, EvalConfig, Evaluator, Object, Span};
use crate::evaluator::objects::{Argument, ArgumentList, ArgumentType, ObjectTy};

#[derive(Clone)]
pub struct Function {
    pub span: Span,
    pub params: Rc<[Identifier]>,
    pub body: BlockExpression,
    pub env: Environment,
}

impl Callable for Function {
    fn call(
        &self,
        _obj: Option<Object>,
        args: Vec<Argument>,
        _env: &mut Environment,
        config: Rc<EvalConfig>,
        call_span: Span,
    ) -> Result<Object, Box<crate::evaluator::Error>> {
        Evaluator::eval_function(self.clone(), args, config, self.body.span.clone(), call_span)
    }

    fn args_len(&self) -> RangeInclusive<usize> {
        self.params.len()..=self.params.len()
    }

    fn argument_list(&self) -> Option<ArgumentList> {
        Some(ArgumentList::new_exactly(self.params.iter().map(|param| ArgumentType { name: param.value.as_ref().into(), ty: ObjectTy::Any }).collect()))
    }
}

pub trait Callable {
    fn call(&self,
            obj: Option<Object>,
            args: Vec<Argument>,
            env: &mut Environment,
            config: Rc<EvalConfig>,
            call_span: Span,
    ) -> Result<Object, Box<crate::evaluator::Error>>;
    
    /// Returns the number of arguments the function takes
    /// Returns `None` if the amount of arguments is unknown
    fn args_len(&self) -> RangeInclusive<usize>;
    
    /// Returns the types of the arguments the function takes
    /// Returns `None` if the types of arguments are unknown
    fn argument_list(&self) -> Option<ArgumentList>;
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
