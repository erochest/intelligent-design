use pretty_assertions::assert_eq;
use rand::prelude::*;

use crate::{token::Token, error::Error};
use super::{Interpreter, Stack};

use Token::*;

#[test]
fn stack_new_creates_an_empty_stack_with_capacity() {
    let stack: Stack<isize> = Stack::new(12);
    assert_eq!(0, stack.len());
    assert_eq!(12, stack.capacity());
}

#[test]
fn stack_push_adds_item() {
    let mut stack = Stack::new(4);
    assert!(stack.push(42).is_ok());
    assert_eq!(1, stack.len());
}

#[test]
fn stack_errors_stack_overflow() {
    let mut stack = Stack::new(2);
    assert!(stack.push(0).is_ok());
    assert!(stack.push(1).is_ok());

    let result = stack.push(2);
    assert!(result.is_err());
    if let Err(Error::StackOverflow) = result {
    } else {
        assert!(false, "stack overflow");
    }
}

#[test]
fn stack_pop_removes_item() {
    let mut stack = Stack::new(24);
    stack.push(1).unwrap();
    stack.push(2).unwrap();
    stack.push(3).unwrap();
    stack.push(5).unwrap();

    assert_eq!(4, stack.len());
    assert_eq!(Some(5), stack.pop());
    assert_eq!(3, stack.len());
}

#[test]
fn stack_peek_returns_stack_top() {
    let mut stack = Stack::new(24);
    stack.push(1).unwrap();
    stack.push(2).unwrap();
    stack.push(3).unwrap();
    stack.push(5).unwrap();
    let expected = 5;

    assert_eq!(4, stack.len());
    assert_eq!(Some(&expected), stack.peek());
    assert_eq!(4, stack.len());
}

#[test]
fn interpreter_interprets_input_program() {
    let mut interp = Interpreter::new();
    let result = interp.execute(vec![IntLiteral(2), IntLiteral(2), Name("+".to_string())]);

    assert!(result.is_ok());
    assert_eq!(Some(IntLiteral(4)), interp.pop())
}

#[test]
fn interpreter_pops_and_pushes() {
    let mut interp = Interpreter::new();
    let value = random();
    assert!(interp.push(IntLiteral(value)).is_ok());
    assert_eq!(1, interp.len());
    assert_eq!(Some(IntLiteral(value)), interp.pop());
}

#[test]
fn interpreter_tracks_stack_size() {
    let mut interp = Interpreter::new();
    assert_eq!(0, interp.len());
    assert!(interp.push(IntLiteral(1)).is_ok());
    assert_eq!(1, interp.len());
    assert!(interp.push(IntLiteral(2)).is_ok());
    assert_eq!(2, interp.len());
    interp.pop();
    assert_eq!(1, interp.len());
    interp.pop();
    assert_eq!(0, interp.len());
}
