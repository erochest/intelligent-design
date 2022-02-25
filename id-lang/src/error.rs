use std::error;
use std::fmt;
use std::io;
use std::num;
use std::result;

pub type Result<R> = result::Result<R, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    IntParseError(num::ParseIntError),
}

use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { 
        match self {
            IoError(ref err) => write!(f, "{}", err),
            IntParseError(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        IoError(err)
    }
}

impl From<num::ParseIntError> for Error {
    fn from(err: num::ParseIntError) -> Self {
        IntParseError(err)
    }
}

#[cfg(test)]
mod tests;