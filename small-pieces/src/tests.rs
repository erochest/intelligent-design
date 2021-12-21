fn random_str() -> String {
    let value: [u8; 32] = rand::random();
    let value = String::from_utf8_lossy(&value).to_string();
    value
}

mod is_atom {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn returns_false_for_cons_cells() {
        assert_that(&Value::Cons(Arc::new(Value::Nil), Arc::new(Value::Nil)).is_atom()).is_false();
    }

    #[test]
    fn returns_true_for_strings() {
        assert_that(&Value::Text("hello sailor".to_string()).is_atom()).is_true();
    }

    #[test]
    fn returns_true_for_numbers() {
        assert_that(&Value::Int(42).is_atom()).is_true();
    }
}

mod is_pair {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn returns_true_for_cons_cells() {
        assert_that(&Value::cons(Value::Nil, Value::Nil).is_pair()).is_true();
    }

    #[test]
    fn returns_false_for_everything_else() {
        assert_that(&Value::Nil.is_pair()).is_false();
        assert_that(&Value::Int(13).is_pair()).is_false();
        assert_that(&Value::Text("greetings".to_string()).is_pair()).is_false();
    }
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

mod is_symbol {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn returns_true_for_symbols() {
        assert_that(&Value::Symbol("name".to_string()).is_symbol()).is_true();
    }

    #[test]
    fn returns_false_for_cons_cells() {
        assert_that(&Value::cons(Value::Nil, Value::Nil).is_symbol()).is_false();
    }

    #[test]
    fn returns_false_for_everything_else() {
        assert_that(&Value::Text("hello".to_string()).is_symbol()).is_false();
    }
}

mod car {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn returns_none_if_not_a_cons_cell() {
        assert_that(&Value::Text("hi".to_string()).car()).is_none();
    }

    #[test]
    fn returns_head_value_if_cons_cell() {
        assert_that(&Value::cons(Value::Int(42), Value::Int(13)).car())
            .is_some()
            .is_equal_to(&Value::Int(42));
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

mod eval {
    use super::super::*;
    use super::*;
    use rand::prelude::*;
    use spectral::prelude::*;

    fn evaluates_to_self(value: Value) {
        let value = Arc::new(value);
        let mut env = Environment::new();
        assert_that(&env.eval(value.clone()))
            .is_ok()
            .is_equal_to(&value);
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

    fn evaluates_to(expr: Value, expected: Value) {
        let expr = Arc::new(expr);
        let expected = Arc::new(expected);
        let mut env = Environment::new();
        assert_that(&env.eval(expr)).is_ok().is_equal_to(&expected);
    }

    #[test]
    fn quote_evaluate_to_cadr() {
        let value = Value::Symbol(random_str());
        let expr = Value::cons(
            Value::symbol("quote"),
            Value::cons(value.clone(), Value::Nil),
        );

        evaluates_to(expr, value);
    }

    #[test]
    fn if_evalutes_to_cadr_when_true() {
        let test = Value::Boolean(true);
        let true_branch = Value::Int(rand::random());
        let false_branch = Value::Int(rand::random());
        let expr = Value::cons(
            Value::symbol("if"),
            Value::cons(
                test.clone(),
                Value::cons(true_branch.clone(), Value::cons(false_branch, Value::Nil)),
            ),
        );

        evaluates_to(expr, true_branch);
    }

    #[test]
    fn if_evaluates_to_cadar_when_false() {
        let test = Value::Boolean(false);
        let true_branch = Value::Int(rand::random());
        let false_branch = Value::Int(rand::random());
        let expr = Value::cons(
            Value::symbol("if"),
            Value::cons(
                test.clone(),
                Value::cons(true_branch, Value::cons(false_branch.clone(), Value::Nil)),
            ),
        );

        evaluates_to(expr, false_branch);
    }

    #[test]
    fn set_adds_value_to_environment() {
        let value: i64 = rand::random();
        let name = "test-var".to_string();
        let expr = Value::cons(
            Value::symbol("set!"),
            Value::cons(
                Value::symbol(name.clone()),
                Value::cons(Value::Int(value), Value::Nil),
            ),
        );
        let mut env = Environment::new();

        assert_that(&env.eval(Arc::new(expr))).is_ok();
        assert_that(&env.lookup(name))
            .is_some()
            .is_equal_to(&Arc::new(Value::Int(value)));
    }

    #[test]
    fn begin_evaluates_its_body_in_the_environment() {
        let value = rand::random();
        let expr = Value::cons(
            Value::symbol("begin"),
            Value::cons(Value::Int(42), Value::cons(Value::Int(value), Value::Nil)),
        );

        evaluates_to(expr, Value::Int(value));
    }

    #[test]
    fn lambda_creates_function() {
        let input = Value::cons(
            Value::symbol("lambda"),
            Value::cons(
                Value::symbol("answer"),
                Value::cons(Value::Nil, Value::cons(Value::Int(42), Value::Nil)),
            ),
        );
        let expected = Value::Fn(
            Arc::new(Value::Nil),
            Arc::new(Value::cons(Value::Int(42), Value::Nil)),
        );

        evaluates_to(input, expected);
    }

    #[test]
    fn invokes_the_named_function() {
        let value: i64 = rand::random();
        let function = Value::Fn(
            Arc::new(Value::Nil),
            Arc::new(Value::cons(Value::Int(value), Value::Nil)),
        );
        let input = Value::cons(Value::Symbol("answer".to_string()), Value::Nil);
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
