use pretty_assertions::assert_eq;

use crate::{parse_id, Token};

#[test]
fn token_from_str_parses_int_literals() {
    let result = "42".parse::<Token>();
    assert!(result.is_ok());
    assert_eq!(Token::IntLiteral(42), result.unwrap());
}

#[test]
fn token_from_str_parses_float_literals() {
    let result = "3.14159".parse::<Token>();
    assert!(result.is_ok());
    if let Token::FloatLiteral(result) = result.unwrap() {
        assert!((3.14159 - result).abs() < 0.001);
    } else {
        assert!(false);
    }
}

#[test]
fn token_from_str_parses_words() {
    let result = "hello".parse::<Token>();
    assert!(result.is_ok());
    assert_eq!(Token::Name("hello".to_string()), result.unwrap());
}

#[test]
fn parse_returns_vec_of_int_literals() {
    let mut input = "1 2 3".as_bytes();
    let result = parse_id(&mut input);

    assert!(result.is_ok());
    let words = result.unwrap();
    assert_eq!(3, words.len());
}
