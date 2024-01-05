use crate::Object;

pub trait ObjectProperties {
    fn get_property(&self, property: &str) -> Option<Object>;
}

impl ObjectProperties for Object {
    fn get_property(&self, property: &str) -> Option<Object> {
        match self {
            Object::Struct(map) => map.get(property).cloned(),
            _ => None,
        }
    }
}
