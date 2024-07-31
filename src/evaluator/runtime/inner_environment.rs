use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::{Mutex};
use crate::evaluator::runtime::environment::{ObjectId};
use crate::Object;

#[derive(Default)]
pub struct InnerEnvironment {
    pub variables: HashMap<Rc<str>, ObjectId>,
    pub heap: Rc<Mutex<HashMap<ObjectId, HeapObject>>>,
    pub outer: Option<Rc<Mutex<InnerEnvironment>>>,
}

#[derive(Debug)]
pub struct HeapObject {
    ref_count: usize,
    obj: Object,
}

impl Deref for HeapObject {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.obj
    }
}

impl DerefMut for HeapObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.obj
    }
}

impl Debug for InnerEnvironment {
    /// Debug implementation for InnerEnvironment
    ///
    /// Prints the store and whether there is an outer environment
    /// does not print the outer environment, as that would cause an infinite loop
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let has_outer = self.outer.is_some();
        write!(
            f,
            "InnerEnvironment {{ store: {{ {:?} }}, has_outer: {} }}",
            self.variables, has_outer
        )
    }
}

impl InnerEnvironment {
    pub fn get_id(&self, name: impl Into<Rc<str>>) -> Option<ObjectId> {
        let name = name.into();
        self.variables.get(&name).copied()
    }

    fn set_object(&mut self, name: Rc<str>, obj: Object) -> ObjectId {
        let id = self.generate_id();
        let mut heap = self.heap.lock().unwrap();
        heap.insert(id, HeapObject { ref_count: 1, obj });
        self.variables.insert(name, id);
        id
    }

    pub(super) fn store_object(&self, object: Object) -> ObjectId {
        let id = self.generate_id();
        let mut heap = self.heap.lock().unwrap();
        heap.insert(id, HeapObject { ref_count: 1, obj: object });
        id
    }

    /// Generates a unique id for a new object
    fn generate_id(&self) -> ObjectId {
        let mut id = ObjectId::random();
        let heap = self.heap.lock().unwrap();
        while heap.contains_key(&id) {
            id = ObjectId::random();
        }
        id
    }

    /// Handles dropping an object by id, decreases the ref count for any objects that are referenced by the object being dropped
    fn handle_drop_for_id(&self, id: ObjectId) {
        let mut heap = self.heap.lock().unwrap();
        if let Some(HeapObject { ref_count, obj }) = heap.get_mut(&id) {
            *ref_count -= 1;
            self.handle_drop_for_object(obj);

            if *ref_count == 0 {
                heap.remove(&id);
            }
        }
    }

    fn handle_drop_for_object(&self, object: &Object) {
        match object {
            Object::Array(arr) => {
                for id in arr {
                    self.handle_drop_for_id(*id);
                }
            }
            Object::Struct(map) => {
                for (_, id) in map {
                    self.handle_drop_for_id(*id);
                }
            }
            Object::Reference(id) => {
                self.handle_drop_for_id(*id);
            },
            _ => {}
        }
    }

    /// Recursively get a value from the environment
    ///
    /// Keeps trying to get the value from the outer environment until it succeeds or there are no more outer
    pub fn get(&self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        match self.variables.get(&name) {
            Some(id) => {
                let heap = self.heap.lock().unwrap();
                let heap_obj = heap.get(id)?;
                Some(heap_obj.obj.clone())
            }
            None => match &self.outer {
                Some(outer) => {
                    let outer_env = outer.lock().unwrap();
                    outer_env.get(name)
                }
                None => None,
            },
        }
    }


    /// Gets a clone of the object with the given id
    pub(super) fn get_by_id(&self, id: ObjectId) -> Option<Object> {
        let heap = self.heap.lock().unwrap();

        heap.get(&id).map(|heap_obj| heap_obj.obj.clone())
    }

    /// Set a value in the current scope
    ///
    /// If a value with the same name already exists, in the current scope
    /// it will be overwritten, if no other reference to the old value exist
    /// it will be dropped
    pub fn set(&mut self, name: impl Into<Rc<str>>, val: Object) {
        let name = name.into();

        if let Some(old_id) = self.variables.get(&name) {
            self.handle_drop_for_id(*old_id);
        }

        self.set_object(name, val);
    }
    
    pub fn set_by_id(&mut self, id: ObjectId, val: Object) {
        let mut heap = self.heap.lock().unwrap();
        let heap_obj = heap.get_mut(&id).unwrap();
        heap_obj.obj = val;
    }

    pub fn update(&mut self, name: impl Into<Rc<str>>, val: Object) -> Result<(), String> {
        let name = name.into();
        match self.variables.get_mut(&name) {
            Some(id) => {
                let mut heap = self.heap.lock().unwrap();
                let heap_obj = heap.get_mut(id).unwrap();
                heap_obj.obj = val;
            }
            None => {
                return match &self.outer {
                    Some(outer) => {
                        let mut outer_env = outer.lock().unwrap();
                        outer_env.update(name, val)
                    }
                    None => Err("Cannot mutate variable that does not exist".to_string()),
                }
            }
        };

        Ok(())
    }

    /// A function to update a value in the environment recursively
    ///
    /// This function takes a name and a function that takes a mutable reference to an Object
    /// It then searches for the value in the environment and applies the function to it
    pub fn mutate(
        &mut self,
        name: impl Into<Rc<str>>,
        func: impl FnOnce(&mut Object) -> Result<Object, String>,
    ) -> Result<Object, String> {
        let name = name.into();
        if let Some(id) = self.variables.get_mut(&name) {
            if let Some(HeapObject { ref mut obj, .. }) = self.heap.lock().unwrap().get_mut(id) {
                func(obj)
            }
            // If the object is not in the heap, return an error
            else {
                Err("Cannot mutate variable that does not exist".to_string())
            }
        } else if let Some(outer) = &self.outer {
            outer.lock().unwrap().mutate(name, func)
        } else {
            Err("Cannot mutate variable that does not exist".to_string())
        }
    }

    /// Delete a value from the environment
    ///
    /// If the value is not in the current scope, it will try to delete it from the outer scope
    /// If the value is not in the outer scope, it will return None
    pub fn delete(&mut self, name: impl Into<Rc<str>>) -> Option<Object> {
        let name = name.into();
        if let Some(id) = self.variables.remove(&name) {
            let mut heap = self.heap.lock().unwrap();
            let heap_obj = heap.remove(&id)?;
            Some(heap_obj.obj)
        } else if let Some(outer) = &self.outer {
            let mut outer_env = outer.lock().unwrap();
            outer_env.delete(name)
        } else {
            None
        }
    }
}

impl Drop for InnerEnvironment {
    fn drop(&mut self) {
        for uuid in self.variables.clone().values() {
            self.handle_drop_for_id(*uuid);
        }
    }
}