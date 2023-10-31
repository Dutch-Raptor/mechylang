use crate::Object;

use super::MethodInner;

pub const FLOAT_METHODS: [MethodInner; 1] = [MethodInner {
    name: "abs",
    args_len: 0..=0,
    function: |obj, _, _, _, _| {
        let float = match obj {
            Object::Float(f) => f,
            _ => return invalid_float_err(),
        };

        Ok(Object::Float(float.abs()))
    },
}];

fn invalid_float_err() -> Result<Object, String> {
    Err("Float method called on non-float object".to_string())
}
