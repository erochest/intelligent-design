use std::collections::HashMap;

use crate::token::Program;

#[derive(Debug)]
pub struct Environment {
    namespace: HashMap<String, Program>,
}

impl Environment {
    pub fn new() -> Self {
        Self { namespace: HashMap::new() }
    }

    pub fn add_name<S: AsRef<str>>(&mut self, name: S, program: Program) {
        let name = name.as_ref().to_string();
        self.namespace.insert(name, program);
    }

    pub fn get<'a, S: AsRef<str>>(&'a self, name: S) -> Option<&'a Program> {
        let name = name.as_ref().to_string();
        self.namespace.get(&name)
    }
}

#[cfg(test)]
mod tests;