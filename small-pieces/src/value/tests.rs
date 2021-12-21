fn random_str() -> String {
    let value: [u8; 32] = rand::random();
    let value = String::from_utf8_lossy(&value).to_string();
    value
}

mod symbol {
    use super::{super::*, random_str};
    use spectral::prelude::*;

    #[test]
    fn returns_symbol_with_input_string() {
        let name = random_str();
        assert_that(&Value::symbol(&name)).is_equal_to(&Value::Symbol(name.clone()));
    }
}

mod invoke {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn returns_none_if_not_fn() {
        let mut env = Environment::new();
        assert_that(&Value::Text("bye".to_string()).invoke(&mut env, &Value::Nil))
            .is_ok()
            .is_none();
    }

    #[test]
    fn returns_result_of_evaluating_body_on_fn() {
        let mut env = Environment::new();
        let expr = Value::Fn(
            Arc::new(Value::Nil),
            Arc::new(Value::cons(
                Value::Int(0),
                Value::cons(Value::Int(1), Value::cons(Value::Int(2), Value::Nil)),
            )),
        );

        assert_that(&expr.invoke(&mut env, &Value::Nil))
            .is_ok()
            .is_some()
            .is_equal_to(&Arc::new(Value::Int(2)));
    }
}
