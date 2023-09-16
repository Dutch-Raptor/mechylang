use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use super::objects::Object;

#[derive(Debug, PartialEq)]
pub struct Environment {
    internal: Rc<InternalEnvironment>,
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Self {
            internal: Rc::clone(&self.internal),
        }
    }
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self {
            internal: Rc::new(InternalEnvironment {
                store: HashMap::new(),
                outer: None,
            }),
        }
    }

    pub(crate) fn new_enclosed(outer: Environment) -> Self {
        Self {
            internal: Rc::new(InternalEnvironment {
                store: HashMap::new(),
                outer: Some(outer),
            }),
        }
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.internal)
    }

    pub fn outer_strong_count(&self) -> usize {
        if let Some(outer) = &self.internal.outer {
            outer.strong_count()
        } else {
            0
        }
    }
}

impl Deref for Environment {
    type Target = InternalEnvironment;

    fn deref(&self) -> &Self::Target {
        &self.internal
    }
}

impl DerefMut for Environment {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Rc::make_mut(&mut self.internal)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InternalEnvironment {
    pub store: HashMap<Rc<str>, Object>,
    outer: Option<Environment>,
}

impl InternalEnvironment {
    pub(crate) fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        self.store
            .get(&name)
            .cloned()
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.get(name)))
    }

    pub(crate) fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        self.store.insert(name.into(), val);
    }
}
