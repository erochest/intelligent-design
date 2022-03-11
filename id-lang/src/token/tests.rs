use pretty_assertions::assert_eq;

use crate::token::{parse_id, Token};

#[test]
fn is_int_returns_true_for_ints() {
    assert!(Token::IntLiteral(42).is_int());
}

#[test]
fn is_int_returns_false_for_everything_else() {
    assert!(!Token::FloatLiteral(3.1415).is_int());
    assert!(!Token::Name("hi".to_string()).is_int());
}

#[test]
fn is_float_returns_false_for_ints_names() {
    assert!(!Token::IntLiteral(42).is_float());
    assert!(!Token::Name("bye".to_string()).is_float());
}

#[test]
fn is_float_returns_true_for_floats() {
    assert!(Token::FloatLiteral(3.1415).is_float());
}

#[test]
fn is_name_returns_false_for_int_float() {
    assert!(!Token::IntLiteral(13).is_name());
    assert!(!Token::FloatLiteral(1.4).is_name());
}

#[test]
fn is_name_returns_true_for_names() {
    assert!(Token::Name("wave".to_string()).is_name());
}

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
fn parse_id_returns_vec_of_tokens() {
    let mut input = "1 2 + .".as_bytes();
    let result = parse_id(&mut input);

    assert!(result.is_ok());
    let words = result.unwrap();
    assert_eq!(4, words.len());
    assert_eq!(
        vec![
            Token::IntLiteral(1),
            Token::IntLiteral(2),
            Token::Name("+".to_string()),
            Token::Name(".".to_string())
        ],
        words
            .into_iter()
            .filter_map(|result| result.ok())
            .collect::<Vec<_>>(),
    );
}
