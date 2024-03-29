use pretty_assertions::assert_eq;

use crate::token::Token;
use crate::environment::{Environment, Executable};

use Token::*;

#[test]
fn environment_can_add_function() {
    let mut env = Environment::new();
    env.add_program(
        "double",
        vec![IntLiteral(2), Name("+".to_string())],
    );
}

#[test]
fn environment_can_retrieve_function() {
    let mut env = Environment::new();
    let expected = Executable::Prog(vec![IntLiteral(2), Name("+".to_string())]);

    env.add_program("double", vec![IntLiteral(2), Name("+".to_string())]);
    let result = env.get("double");

    assert!(result.is_some());
    let result = result.unwrap();
    match result {
        &Executable::Prog(ref p) => assert!(p == &vec![IntLiteral(2), Name("+".to_string())]),
        _ => assert!(false),
    }
}

#[test]
fn environment_returns_none_for_name_not_found() {
    let env = Environment::new();
    let result = env.get("name");
    assert!(result.is_none());
}