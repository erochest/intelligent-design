use std::str::FromStr;
use std::io::Read;
use std::result;

mod error;

use crate::error::{Error, Result};

// TODO: Rename Word to Token
// TODO: Add Program type

#[derive(Debug, PartialEq)]
pub enum Word {
    IntLiteral(isize),
    FloatLiteral(f64),
    Name(String),
}

impl FromStr for Word {
    type Err = Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        if let Ok(literal) = s.parse() {
            return Ok(Word::IntLiteral(literal));
        } else if let Ok(literal) = s.parse() {
            return Ok(Word::FloatLiteral(literal));
        } else {
            return Ok(Word::Name(s.to_string()));
        }
    }
}

pub fn parse_id<R: Read>(input: &mut R) -> Result<Vec<Result<Word>>> {
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
