use std::collections::HashMap;

use super::object::Object;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.get(name).cloned()
    }
}
