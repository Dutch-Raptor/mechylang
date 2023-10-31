use crate::Object;

use super::MethodInner;

pub const BOOLEAN_METHODS: [MethodInner; 1] = [MethodInner {
    name: "not",
    args_len: 0..=0,
    function: |obj, _, _, _, _| {
        let boolean = match obj {
            Object::Boolean(b) => b,
            _ => return invalid_boolean_err(),
        };

        Ok(Object::Boolean(!boolean))
    },
}];

fn invalid_boolean_err() -> Result<Object, String> {
    Err("Boolean method called on non-boolean object".to_string())
}
