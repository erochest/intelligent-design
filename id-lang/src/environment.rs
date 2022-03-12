use std::collections::HashMap;
use std::fmt::Debug;

use rand::rngs::adapter::ReseedingRng;

use crate::token::Program;
use crate::words::{Op, PlusOp};

pub enum Executable {
    Prog(Program),
    Oper(Box<dyn Op>),
}

pub struct Environment {
    namespace: HashMap<String, Executable>,
}

impl Environment {
    pub fn new() -> Self {
        Self { namespace: HashMap::new() }
    }

    pub fn load_std(&mut self) {
        self.namespace.insert("+".to_string(), Executable::Oper(Box::new(PlusOp::new())));
    }

    pub fn add_program<S: AsRef<str>>(&mut self, name: S, program: Program) {
        let name = name.as_ref().to_string();
        self.namespace.insert(name, Executable::Prog(program));
    }

    pub fn get<'a, S: AsRef<str>>(&'a self, name: S) -> Option<&'a Executable> {
        let name = name.as_ref().to_string();
        self.namespace.get(&name)
    }
}

#[cfg(test)]
mod tests;