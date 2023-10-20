use std::ops::RangeInclusive;

use crate::parser::expressions::Identifier;

use super::{environment::Environment, eval::Evaluator, objects::Object};

/// A builtin function that can be called from `mechylang`
#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunction {
    /// The name of the builtin function
    ///
    /// This is the name that will be used to call the function
    pub name: &'static str,
    /// The number of arguments the function takes
    pub args_len: RangeInclusive<usize>,
    /// The function that will be called when the builtin is called
    ///
    /// # Arguments
    /// - `args`: The arguments passed to the function
    /// - `env`: The environment the function is being called in
    /// - `eval`: The evaluator, used for configuration (e.g. printing)
    pub function: fn(&Vec<Object>, &mut Environment, &Evaluator) -> BuiltinResult,
}

type BuiltinResult = Result<Object, (String, BuiltinError)>;

/// A list of all the builtin functions in `mechylang`
pub const BUILTINS: [BuiltinFunction; 2] = [
    BuiltinFunction {
        name: "len",
        args_len: (1..=1),
        function: |args, _, _| match args[0] {
            Object::String(ref s) => Ok(Object::Integer(s.len() as i64)),
            Object::Array(ref a) => Ok(Object::Integer(a.len() as i64)),
            _ => Err((
                format!("Argument to `len` not supported, got {:?}", args[0]),
                BuiltinError::WrongArgumentType,
            )),
        },
    },
    BuiltinFunction {
        name: "print",
        args_len: (1..=usize::MAX),
        function: |args, _, eval| {
            eval.print(format!(
                "{}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ));
            Ok(Object::Null)
        },
    },
];

#[derive(Debug, PartialEq)]
pub enum BuiltinError {
    WrongArgumentType,
}

impl TryFrom<&Identifier> for BuiltinFunction {
    type Error = ();
    fn try_from(ident: &Identifier) -> Result<Self, ()> {
        BUILTINS
            .iter()
            .find(|b| *b.name == *ident.value)
            .cloned()
            .ok_or(())
    }
}

impl From<BuiltinFunction> for Object {
    fn from(builtin: BuiltinFunction) -> Self {
        Object::BuiltinFunction(builtin)
    }
}
