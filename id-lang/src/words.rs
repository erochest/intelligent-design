use std::fmt;

use crate::error::{Error, Result};
use crate::interpreter::Stack;
use crate::token::{self, Token};

pub type StackOp = Box<dyn Fn(&mut Stack<Token>) -> Result<()>>;

pub trait Op {
    fn word(&self) -> &String;
    fn check(&self, stack: &Stack<Token>) -> Result<()>;
    fn execute(&self, stack: &mut Stack<Token>) -> Result<()>;
}

pub struct PlusOp {
    word: String,
}

impl PlusOp {
    pub fn new() -> Self {
        PlusOp {
            word: "+".to_string(),
        }
    }
}

impl Op for PlusOp {
    fn word(&self) -> &String {
        &self.word
    }

    fn check(&self, stack: &Stack<Token>) -> Result<()> {
        if stack.len() < 2 {
            return Err(Error::StackUnderflow);
        }

        let is_int_a = stack.dip(1).map(|t| t.is_int()).unwrap_or(false);
        let is_int_b = stack.dip(0).map(|t| t.is_int()).unwrap_or(false);
        if (!is_int_a) || (!is_int_b) {
            return Err(Error::TypeError("integer".to_string()));
        }

        Ok(())
    }

    fn execute(&self, stack: &mut Stack<Token>) -> Result<()> {
        if let Some(Token::IntLiteral(b)) = stack.pop() {
            if let Some(Token::IntLiteral(a)) = stack.pop() {
                stack.push(Token::IntLiteral(plus_op(a, b)));
            }
        }
        Ok(())
    }
}

// #[id_op("PlusOp", "+", "i i -- i")]
pub fn plus_op(a: isize, b: isize) -> isize {
    a + b
}

#[cfg(test)]
mod tests;
