use std::str::FromStr;
use std::io::Read;
use std::result;

mod error;

use crate::error::{Error, Result};

#[derive(Debug, PartialEq)]
pub enum Word {
    IntLiteral(isize),
}

impl FromStr for Word {
    type Err = Error;

    fn from_str(s: &str) -> result::Result<Self, Self::Err> {
        let literal: isize = s.parse()?;
        Ok(Word::IntLiteral(literal))
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
