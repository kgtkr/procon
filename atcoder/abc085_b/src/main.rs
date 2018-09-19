extern crate core;

use std::collections::HashSet;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<HashSet<_>>();
    list.len().to_string()
}

#[test]
fn test() {
    let tests = vec![("4\n10\n8\n8\n6", "3")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
