use super::objects::{BuiltinFunction, Object};

pub const BUILTINS: [BuiltinFunction; 2] = [
    BuiltinFunction {
        name: "len",
        args_len: (1..=1),
        function: |args| match args[0] {
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
        function: |args| {
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
];

#[derive(Debug, PartialEq)]
pub enum BuiltinError {
    WrongArgumentType,
}
