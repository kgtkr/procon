extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn l(i: i32) -> i64 {
    match i {
        0 => 2,
        1 => 1,
        _ => l(i - 1) + l(i - 2),
    }
}

fn run(input: String) -> String {
    let i = input.parse::<i32>().unwrap();

    l(i).to_string()
}

#[test]
fn test() {
    let tests = vec![("5", "11"), ("86", "939587134549734843")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
