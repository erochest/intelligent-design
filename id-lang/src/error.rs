use std::error;
use std::fmt;
use std::io;
use std::result;

pub type Result<R> = result::Result<R, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    WordParseError(String),
}

use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self {
            IoError(ref err) => write!(f, "{}", err),
            WordParseError(ref input) => write!(f, "unable to determine word for: {}", input),
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