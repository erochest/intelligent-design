fn random_str() -> String {
    let value: [u8; 32] = rand::random();
    let value = String::from_utf8_lossy(&value).to_string();
    value
}

mod eval {
    use super::super::*;
    use super::*;
    use spectral::prelude::*;

    fn evaluates_to_self(value: Value) {
        let value = Arc::new(value);
        let mut env = Environment::new();
        assert_that(&env.eval(value.clone()))
            .is_ok()
            .is_equal_to(&value);
    }

    fn evaluates_to(expr: Value, expected: Value) {
        let expr = Arc::new(expr);
        let expected = Arc::new(expected);
        let mut env = Environment::new();
        assert_that(&env.eval(expr)).is_ok().is_equal_to(&expected);
    }

    #[test]
    fn numbers_evaluate_to_themselves() {
        evaluates_to_self(Value::Int(rand::random()));
    }

    #[test]
    fn strings_evaluate_to_themselves() {
        evaluates_to_self(Value::Text(random_str()));
    }

    #[test]
    fn chars_evaluate_to_themselves() {
        evaluates_to_self(Value::Char(rand::random()));
    }

    #[test]
    fn booleans_evaluate_to_themselves() {
        evaluates_to_self(Value::Boolean(rand::random()));
    }

    #[test]
    fn vectors_evaluates_to_themselves() {
        let value: [i64; 32] = rand::random();
        let value = Vec::from(value)
            .into_iter()
            .map(Value::Int)
            .collect::<Vec<_>>();
        evaluates_to_self(Value::Vector(value));
    }

    #[test]
    fn quote_evaluate_to_cadr() {
        let value = Value::Symbol(random_str());
        let expr = vec!["quote".into(), value.clone()].into();
        evaluates_to(expr, value);
    }

    #[test]
    fn if_evalutes_to_cadr_when_true() {
        let test = Value::Boolean(true);
        let true_branch = Value::Int(rand::random());
        let false_branch = Value::Int(rand::random());
        let expr = vec![
            "if".into(),
            test.clone(),
            true_branch.clone(),
            false_branch.clone(),
        ]
        .into();

        evaluates_to(expr, true_branch);
    }

    #[test]
    fn if_evaluates_to_cadar_when_false() {
        let test = Value::Boolean(false);
        let true_branch = Value::Int(rand::random());
        let false_branch = Value::Int(rand::random());
        let expr = vec![
            "if".into(),
            test.clone(),
            true_branch.clone(),
            false_branch.clone(),
        ]
        .into();

        evaluates_to(expr, false_branch);
    }

    #[test]
    fn set_adds_value_to_environment() {
        let value: i64 = rand::random();
        let name = "test-var".to_string();
        let expr = vec![Value::from("set!"), name.clone().into(), value.into()].into();
        let mut env = Environment::new();

        assert_that(&env.eval(Arc::new(expr))).is_ok();
        assert_that(&env.lookup(name))
            .is_some()
            .is_equal_to(&Arc::new(Value::Int(value)));
    }

    #[test]
    fn begin_evaluates_its_body_in_the_environment() {
        let value: i64 = rand::random();
        let expr = vec![Value::from("begin"), 42.into(), value.into()].into();

        evaluates_to(expr, Value::Int(value));
    }

    #[test]
    fn lambda_creates_function() {
        let input = vec![Value::from("lambda"), Vec::<Value>::new().into(), 42.into()].into();
        let expected = Value::Fn(Arc::new(Value::Nil), Arc::new(vec![42].into()));

        evaluates_to(input, expected);
    }

    #[test]
    fn invokes_the_named_function() {
        let value: i64 = rand::random();
        let function = Value::Fn(Arc::new(Value::Nil), Arc::new(vec![42, value].into()));
        let input = vec!["answer"].into();
        let expected = Value::Int(value);
        let mut env = Environment::new();

        env.set("answer", &Arc::new(function)).unwrap();

        assert_that(&env.eval(Arc::new(input)))
            .is_ok()
            .is_equal_to(&Arc::new(expected));
    }
}

mod set_var {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn sets_value() {
        let mut env = Environment::new();
        assert_that(&env.set("name", &Arc::new(Value::Int(rand::random())))).is_ok();
    }
}

mod lookup {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn looks_up_a_symbol_in_an_environment() {
        let mut env = Environment::new();
        let value: i64 = rand::random();
        assert_that(&env.set("name", &Arc::new(Value::Int(value)))).is_ok();
        assert_that(&env.lookup("name"))
            .is_some()
            .is_equal_to(&Arc::new(Value::Int(value)));
    }
}
