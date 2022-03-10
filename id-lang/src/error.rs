use std::error;
use std::fmt;
use std::io;
use std::result;

pub type Result<R> = result::Result<R, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    WordParseError(String),
    StackOverflow,
    StackUnderflow,
    TypeError(String),
}

use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self {
            IoError(ref err) => write!(f, "{}", err),
            WordParseError(ref input) => write!(f, "unable to determine word for: {}", input),
            StackOverflow => write!(f, "stack overflow"),
            StackUnderflow => write!(f, "stack underflow"),
            TypeError(ref expected) => write!(f, "type error: expected {:?}", expected),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        IoError(err)
    }
}

#[cfg(test)]
mod tests;