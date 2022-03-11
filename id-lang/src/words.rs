use std::fmt;

use crate::error::{Result, Error};
use crate::interpreter::Stack;
use crate::token::{Token, self};

pub type StackOp = Box<dyn Fn(&mut Stack<Token>) -> Result<()>>;

pub struct Op {
    pub execute: StackOp,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Op").field("execute", &"...".to_string()).finish()
    }
}

impl PartialEq for Op {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

pub fn plus_op(stack: &mut Stack<Token>) -> Result<()> {
    if stack.len() < 2 {
        return Err(Error::StackUnderflow);
    }

    let is_int_a = stack.dip(1).map(|t| t.is_int()).unwrap_or(false);
    let is_int_b = stack.dip(0).map(|t| t.is_int()).unwrap_or(false);
    if (!is_int_a) || (!is_int_b) {
        return Err(Error::TypeError("integer".to_string()));
    }

    if let Some(Token::IntLiteral(b)) = stack.pop() {
        if let Some(Token::IntLiteral(a)) = stack.pop() {
            stack.push(Token::IntLiteral(a + b))?;
        }
    }

    Ok(())
}

pub fn plus_op_factory() -> Op {
    Op {
        execute: Box::new(plus_op),
    }
}

#[cfg(test)]
mod tests;
