use pretty_assertions::assert_eq;

use crate::{parse_id, Word};

#[test]
fn word_from_str_parses_int_literals() {
    let result = "42".parse::<Word>();
    assert!(result.is_ok());
    assert_eq!(Word::IntLiteral(42), result.unwrap());
}

#[test]
fn parse_returns_vec_of_int_literals() {
    let mut input = "1 2 3".as_bytes();
    let result = parse_id(&mut input);

    assert!(result.is_ok());
    let words = result.unwrap();
    assert_eq!(3, words.len());
}
