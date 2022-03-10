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
    
    if let Some(token_a) = stack.pop() {
        if let Token::IntLiteral(a) = token_a {
            if let Some(token_b) = stack.pop() {
                if let Token::IntLiteral(b) = token_b {
                    stack.push(Token::IntLiteral(a + b))?;
                } else {
                    stack.push(token_b)?;
                    stack.push(token_a)?;
                    return Err(Error::TypeError("integer".to_string()));
                }
            } else {
                stack.push(token_a)?;
            }
        } else {
            stack.push(token_a)?;
            return Err(Error::TypeError("integer".to_string()));
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
