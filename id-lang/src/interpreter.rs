use crate::environment::{Environment, Executable};
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

    pub fn dip(&self, offset: usize) -> Option<&T> {
        self.data.get(self.data.len() - offset - 1)
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
        let mut env = Environment::new();
        let stack = Stack::new(capacity);
        env.load_std();

        Self {
            env,
            stack,
        }
    }

    pub fn execute(&mut self, program: Program) -> Result<()> {
        for item in program {
            eprintln!("evaluating {:?}", item);
            match item {
                Token::IntLiteral(_) => self.stack.push(item)?,
                Token::FloatLiteral(_) => self.stack.push(item)?,
                Token::Name(name) => self.evaluate(&name)?,
            }
        }
        Ok(())
    }

    pub fn evaluate<S: AsRef<str>>(&mut self, name: S) -> Result<()> {
        if let Some(exec) = self.env.get(&name) {
            match exec {
                Executable::Prog(_) => todo!(),
                Executable::Oper(op) => {
                    let op = &op.execute;
                    op(&mut self.stack)?;
                }
            }
        }
        // TODO: eating this error.
        Ok(())
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
