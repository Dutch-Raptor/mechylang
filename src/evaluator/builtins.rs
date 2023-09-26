use crate::parser::expressions::{Expression, Identifier};

use super::objects::{BuiltinFunction, Object};

pub const BUILTINS: [BuiltinFunction; 3] = [
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
        function: |args, _, _| {
            println!(
                "{}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            Ok(Object::Null)
        },
    },
    // Naive push implementation
    BuiltinFunction {
        name: "push",
        args_len: (2..=2),
        function: |arg_objs, arg_exprs, env| match arg_objs[0] {
            Object::Array(_) => {
                // get the identifier of the array
                let ident = match arg_exprs[0] {
                    Expression::Identifier(ref ident) => ident,
                    _ => {
                        return Err((
                            "Expected identifier".to_string(),
                            BuiltinError::WrongArgumentType,
                        ))
                    }
                };

                // get the value to push
                let value = arg_objs[1].clone();

                env.update(ident.value.clone(), move |arr| {
                    if let Object::Array(ref mut arr) = arr {
                        arr.push(value.clone());
                        Ok(())
                    } else {
                        Err(format!("Expected array, got {}", arr))
                    }
                })
                .map_err(|e| (e, BuiltinError::WrongArgumentType))?;

                Ok(Object::Null)
            }
            _ => Err((
                format!("Argument to `push` not supported, got {:?}", arg_objs[0]),
                BuiltinError::WrongArgumentType,
            )),
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
