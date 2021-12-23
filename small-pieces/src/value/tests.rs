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
            Arc::new(Vec::<Value>::new().into()),
            Arc::new(vec![0, 1, 2].into()),
        );

        assert_that(&expr.invoke(&mut env, &Value::Nil))
            .is_ok()
            .is_some()
            .is_equal_to(&Arc::new(Value::Int(2)));
    }
}

mod from {
    use super::{super::*, random_str};
    use spectral::prelude::*;

    #[test]
    fn returns_boolean_variant() {
        assert_that(&Value::from(true)).is_equal_to(&Value::Boolean(true));
        assert_that(&Value::from(false)).is_equal_to(&Value::Boolean(false));
    }

    #[test]
    fn returns_char_variant() {
        assert_that(&Value::from('a')).is_equal_to(&Value::Char('a'));
        assert_that(&Value::from('X')).is_equal_to(&Value::Char('X'));

        let c: char = rand::random();
        assert_that(&Value::from(c)).is_equal_to(&Value::Char(c));
    }

    #[test]
    fn returns_empty_cons_for_empty_vector() {
        let input: Vec<Value> = vec![];
        assert_that(&Value::from(input)).is_equal_to(&Value::EmptyCons);
    }

    #[test]
    fn returns_cons_list_for_vector() {
        let input: Vec<Value> = vec![Value::Int(2), Value::Int(13), Value::Int(42)];
        let expected = Value::cons(
            Value::Int(2),
            Value::cons(
                Value::Int(13),
                Value::cons(Value::Int(42), Value::EmptyCons),
            ),
        );

        assert_that(&Value::from(input)).is_equal_to(&expected);
    }

    #[test]
    fn returns_cons_list_for_raw_values() {
        let input = vec![false, true, true, false];
        let expected = Value::cons(
            Value::Boolean(false),
            Value::cons(
                Value::Boolean(true),
                Value::cons(
                    Value::Boolean(true),
                    Value::cons(Value::Boolean(false), Value::EmptyCons),
                ),
            ),
        );

        assert_that(&Value::from(input)).is_equal_to(&expected);
    }

    #[test]
    fn returns_ints() {
        let value: i64 = rand::random();
        assert_that(&Value::from(value)).is_equal_to(&Value::Int(value));
    }

    #[test]
    fn returns_symbols_for_str() {
        let value = "list";
        assert_that(&Value::from(value)).is_equal_to(&Value::Symbol("list".to_string()));
    }

    #[test]
    fn returns_symbols_for_strings() {
        let value = random_str();
        assert_that(&Value::from(value.clone())).is_equal_to(&Value::Symbol(value.clone()));
    }
}

mod iter {
    use super::super::*;
    use spectral::prelude::*;

    #[test]
    fn iterates_over_the_contents_of_the_list() {
        let value: i64 = rand::random();
        let input: Value = vec![0, 1, 1, 2, 3, 5, 8, 13, 21, value].into();
        let expected: Vec<SharedValue> = vec![
            Arc::new(0.into()),
            Arc::new(1.into()),
            Arc::new(1.into()),
            Arc::new(2.into()),
            Arc::new(3.into()),
            Arc::new(5.into()),
            Arc::new(8.into()),
            Arc::new(13.into()),
            Arc::new(21.into()),
            Arc::new(value.into()),
        ];

        assert_that(&input.into_iter().collect::<Vec<_>>()).is_equal_to(&expected);
    }
}
