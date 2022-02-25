use std::fs;

use pretty_assertions::assert_eq;

use super::Error;

#[test]
fn from_converts_io_error() {
    let error = fs::File::open("does-not-exist");
    assert!(error.is_err());
    let error = error.unwrap_err();
    let error: Error = error.into();
}

#[test]
fn from_convert_parse_int_error() {
    let error = "hello".parse::<isize>();
    assert!(error.is_err());
    let error = error.unwrap_err();
    let error: Error = error.into();
}

#[test]
fn fmt_displays_io_errors() {
    let error: Error = fs::File::open("does-not-exist").unwrap_err().into();
    let display = format!("{}", error);
    assert!(! display.is_empty());
}

#[test]
fn fmt_displays_int_parse_errors() {
    let error: Error = "hello".parse::<isize>().unwrap_err().into();
    let display = format!("{}", error);
    assert!(! display.is_empty());
}