use std::convert::From;
use std::iter::{Iterator, IntoIterator};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use crate::environment::Environment;
use crate::error::{Error, Result};

// TODO: function parameters in `lambda` and `invoke`

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Char(char),
    Cons(SharedValue, SharedValue),
    EmptyCons,
    Fn(SharedValue, SharedValue),
    Int(i64),
    Symbol(String),
    Text(String),
    Vector(Vec<Value>),
    Empty,
}

#[cfg(not(feature = "concurrent"))]
pub type Shared<T> = Rc<T>;

#[cfg(feature = "concurrent")]
pub type Shared<T> = Arc<T>;

#[derive(Debug, PartialEq, Eq)]
pub struct SharedValue(Shared<Value>);

impl SharedValue {
    pub fn new(value: Value) -> Self {
        SharedValue(Shared::new(value))
    }
}

impl Clone for SharedValue {
    fn clone(&self) -> Self {
        Self(Shared::clone(&self.0))
    }
}

impl<V: Into<Value>> From<V> for SharedValue {
    fn from(value: V) -> Self {
        SharedValue::new(value.into())
    }
}

impl AsRef<Value> for SharedValue {
    fn as_ref(&self) -> &Value {
        self.0.as_ref()
    }
}

impl Deref for SharedValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

use Value::*;

impl Value {
    pub fn cons(head: Value, tail: Value) -> Self {
        Cons(head.into(), tail.into())
    }

    pub fn symbol<S: AsRef<str>>(symbol: S) -> Self {
        Symbol(symbol.as_ref().to_string())
    }

    pub fn invoke(&self, env: &mut Environment, _params: &Value) -> Result<Option<SharedValue>> {
        if let Fn(_, body) = self {
            let mut result = None;
            let mut current = body.clone();

            while let Value::Cons(head, tail) = current.as_ref() {
                result = Some(env.eval(head.clone())?);
                current = tail.clone();
            }

            Ok(result)
        } else {
            Ok(None)
        }
    }

    pub fn eval_error(&self) -> Error {
        Error::EvaluationError(format!("unable to evaluate: {:?}", self))
    }
}

impl Default for Value {
    fn default() -> Self {
        Nil
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Boolean(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Char(value)
    }
}

impl<V: Into<Value>> From<Vec<V>> for Value {
    fn from(values: Vec<V>) -> Self {
        values
            .into_iter()
            .rev()
            .fold(EmptyCons, |current, item| Value::cons(item.into(), current))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Int(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Symbol(value.to_string())
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Symbol(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ConsIter {
    cursor: SharedValue,
}

impl IntoIterator for SharedValue {
    type Item = SharedValue;
    type IntoIter = ConsIter;

    fn into_iter(self) -> Self::IntoIter {
        ConsIter { cursor: self }
    }
}

impl Iterator for ConsIter {
    type Item = SharedValue;

    fn next(&mut self) -> Option<Self::Item> {
        if let Cons(head, tail) = self.cursor.clone().as_ref() {
            self.cursor = (*tail).clone();
            Some(head.clone())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests;
