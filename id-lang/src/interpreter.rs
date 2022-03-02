use crate::environment::Environment;
use crate::error::{Error, Result};
use crate::token::{Program, Token};

pub struct Stack<T> {
    data: Vec<T>,
}

impl<T> Stack<T> {
    pub fn new(capacity: usize) -> Self {
        let data = Vec::with_capacity(capacity);
        Self { data }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }

    pub fn push(&mut self, token: T) -> Result<()> {
        if self.data.len() < self.data.capacity() {
            self.data.push(token);
            Ok(())
        } else {
            Err(Error::StackOverflow)
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop()
    }

    pub fn peek(&self) -> Option<&T> {
        self.data.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }
}

pub struct Interpreter {
    env: Environment,
    stack: Stack<Token>,
}

pub const DEFAULT_STACK_CAPACITY: usize = 1024;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter::with_capacity(DEFAULT_STACK_CAPACITY)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let stack = Stack::new(capacity);
        Self {
            env: Environment::new(),
            stack,
        }
    }

    pub fn execute(&mut self, program: Program) -> Result<()> {
        Ok(())
    }

    pub fn evaluate<S: AsRef<str>>(&mut self, name: S) -> Result<()> {
        todo!()
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn push(&mut self, token: Token) -> Result<()> {
        self.stack.push(token)
    }

    pub fn pop(&mut self) -> Option<Token> {
        self.stack.pop()
    }
}

#[cfg(test)]
mod tests;
