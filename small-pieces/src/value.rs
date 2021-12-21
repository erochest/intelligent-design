use std::sync::Arc;

use crate::error::{Error, Result};
use crate::environment::Environment;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Char(char),
    Cons(SharedValue, SharedValue),
    Fn(SharedValue, SharedValue),
    Int(i64),
    Symbol(String),
    Text(String),
    Vector(Vec<Value>),
}

pub type SharedValue = Arc<Value>;

use Value::*;

// Will the type predicates below be used in practice? Or will matching cover them?

impl Value {
    pub fn cons(head: Value, tail: Value) -> Self {
        Cons(Arc::new(head), Arc::new(tail))
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

#[cfg(test)]
mod tests;