use std::str::FromStr;
use std::io::Read;
use std::result;

use crate::error::{Error, Result};

#[derive(Debug, PartialEq)]
pub enum Token {
    IntLiteral(isize),
    FloatLiteral(f64),
    Name(String),
}

impl FromStr for Token {
    type Err = Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        if let Ok(literal) = s.parse() {
            return Ok(Token::IntLiteral(literal));
        } else if let Ok(literal) = s.parse() {
            return Ok(Token::FloatLiteral(literal));
        } else {
            return Ok(Token::Name(s.to_string()));
        }
    }
}

type Program = Vec<Token>;

pub fn parse_id<R: Read>(input: &mut R) -> Result<Vec<Result<Token>>> {
    let mut buffer = String::new();
    input.read_to_string(&mut buffer)?;
    let words = buffer
        .split_ascii_whitespace()
        .map(|buf| buf.parse())
        .collect();
    Ok(words)
}

#[cfg(test)]
mod tests;
