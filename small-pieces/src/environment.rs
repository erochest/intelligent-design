use std::collections::HashMap;
use std::sync::Arc;

use crate::error::*;
use crate::value::*;

use Value::*;

pub struct Environment {
    variables: HashMap<String, SharedValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    pub fn eval<'a>(&'a mut self, expr: SharedValue) -> Result<SharedValue> {
        match expr.as_ref() {
            Int(_) | Text(_) | Char(_) | Boolean(_) | Vector(_) => Ok(expr),
            Cons(ref symbol, tail) => {
                if let Symbol(symbol) = symbol.as_ref() {
                    if let Some(result) = self.eval_cons(symbol, &tail) {
                        result
                    } else {
                        Err(expr.eval_error())
                    }
                } else {
                    Err(expr.eval_error())
                }
            }
            _ => Err(expr.eval_error()),
        }
    }

    fn eval_cons<'a, S: AsRef<str>>(
        &'a mut self,
        symbol: S,
        tail: &Value,
    ) -> Option<Result<SharedValue>> {
        match symbol.as_ref() {
            "begin" => self.eval_begin(tail),
            "if" => self.eval_if(tail),
            "lambda" => self.eval_lambda(tail),
            "quote" => self.eval_quote(tail),
            "set!" => self.eval_set(tail),
            _ => self.eval_invoke(symbol, tail),
        }
    }

    fn eval_quote<'a>(&'a mut self, tail: &Value) -> Option<Result<SharedValue>> {
        if let Cons(head, _) = tail {
            Some(Ok(head.clone()))
        } else {
            None
        }
    }

    fn eval_if<'a>(&'a mut self, tail: &Value) -> Option<Result<SharedValue>> {
        if let Cons(test_expr, tail) = tail {
            let test_result = self.eval(test_expr.clone());
            match test_result {
                Ok(test_result) => {
                    let test_result = test_result.clone();
                    if let Boolean(false) = test_result.as_ref() {
                        if let Cons(_, false_branch) = tail.as_ref() {
                            let false_branch = false_branch.as_ref();
                            match false_branch {
                                Cons(false_branch, _) => {
                                    return Some(self.eval(false_branch.clone()));
                                },
                                EmptyCons => {
                                    return Some(Ok(Nil.into()));
                                }
                                _ => {}
                            }
                        }
                    } else {
                        if let Cons(true_branch, _) = tail.as_ref() {
                            return Some(self.eval(true_branch.clone()));
                        }
                    }
                }
                Err(_) => return Some(test_result),
            }
        }
        None
    }

    fn eval_set<'a>(&'a mut self, tail: &Value) -> Option<Result<SharedValue>> {
        if let Cons(name_symbol, tail) = tail {
            if let Symbol(name) = name_symbol.as_ref() {
                if let Cons(value_expr, _) = tail.as_ref() {
                    let value_result = self.eval(value_expr.clone());
                    match value_result {
                        Ok(value_result) => {
                            let set_result = self.set(name, &value_result);
                            if let Ok(_) = set_result {
                                return Some(Ok(value_result));
                            } else if let Err(err) = set_result {
                                return Some(Err(err));
                            }
                        }
                        Err(_) => {
                            return Some(value_result);
                        }
                    }
                }
            }
        }
        None
    }

    // This is roughly a translation of `eprogn` from page 10.
    fn eval_begin<'a>(&'a mut self, tail: &Value) -> Option<Result<SharedValue>> {
        if let Cons(head, tail) = tail {
            let head_result = self.eval(head.clone());
            if let Ok(_) = head_result {
                if let Value::EmptyCons = tail.as_ref() {
                    return Some(head_result);
                } else {
                    return self.eval_begin(&tail);
                }
            } else if let Err(_) = head_result {
                return Some(head_result);
            }
        } else if let EmptyCons = tail {
            return Some(Ok(Empty.into()))
        }
        None
    }

    fn eval_lambda<'a>(&'a mut self, tail: &Value) -> Option<Result<SharedValue>> {
        if let Cons(params, body) = tail {
            let lambda = Fn(params.clone(), body.clone());
            return Some(Ok(lambda.into()));
        }
        None
    }

    pub fn set<S: AsRef<str>>(&mut self, name: S, value: &SharedValue) -> Result<()> {
        self.variables
            .insert(name.as_ref().to_string(), value.clone());
        Ok(())
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<SharedValue> {
        self.variables.get(&name.as_ref().to_string()).cloned()
    }

    fn eval_invoke<S: AsRef<str>>(
        &mut self,
        symbol: S,
        tail: &Value,
    ) -> Option<Result<SharedValue>> {
        self.lookup(symbol)
            .and_then(|f| f.invoke(self, tail).transpose())
    }
}

#[cfg(test)]
mod tests;
