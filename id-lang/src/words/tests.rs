use std::fmt::Debug;

use pretty_assertions::assert_eq;

use crate::error::Error;
use crate::token::Token;
use crate::interpreter::Stack;

use Token::*;

use super::{Op, PlusOp};

fn assert_stack<T: PartialEq + Debug>(stack: Stack<T>, expected: &[T]) {
    assert_eq!(expected.len(), stack.len());
    for (e, a) in expected.into_iter().zip(stack.iter()) {
        assert_eq!(e, a);
    }
}

#[test]
fn plus_add_two_integers() {
    let mut stack: Stack<Token> = Stack::new(1024);
    stack.push(IntLiteral(2)).unwrap();
    stack.push(IntLiteral(3)).unwrap();
    let plus = PlusOp::new();

    let result = plus.execute(&mut stack);

    assert!(result.is_ok());
    assert_stack(stack, &[IntLiteral(5)]);
}

#[test]
fn plus_throws_error_on_type_mismatch() {
    let mut stack: Stack<Token> = Stack::new(1024);
    stack.push(IntLiteral(2)).unwrap();
    stack.push(Name("hello".to_string())).unwrap();
    let plus = PlusOp::new();

    let result = plus.check(&mut stack);

    assert!(result.is_err());
    match result {
        Err(Error::TypeError(ref expected)) => assert_eq!(expected, &String::from("integer")),
        _ => assert!(false),
    }
    assert_stack(stack, &[IntLiteral(2), Name("hello".to_string())]);
}

#[test]
fn plus_does_nothing_on_stack_underflow() {
    let mut stack: Stack<Token> = Stack::new(1024);
    stack.push(IntLiteral(2)).unwrap();
    let plus = PlusOp::new();

    let result = plus.check(&mut stack);

    assert!(result.is_err());
    match result {
        Err(Error::StackUnderflow) => assert!(true),
        _ => assert!(false),
    }
    assert_stack(stack, &[IntLiteral(2)]);
}
