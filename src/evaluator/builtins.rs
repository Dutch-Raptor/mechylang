use crate::parser::expressions::{Expression, Identifier};

use super::objects::{BuiltinFunction, Object};

pub const BUILTINS: [BuiltinFunction; 2] = [
    BuiltinFunction {
        name: "len",
        args_len: (1..=1),
        function: |args, _, _, _| match args[0] {
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
        function: |args, _, _, config| {
            let mut out = match config.output.lock() {
                Ok(out) => out,
                Err(_) => {
                    return Err((
                        "Could not lock output".to_string(),
                        BuiltinError::WrongArgumentType,
                    ))
                }
            };
            writeln!(
                out,
                "{}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
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
