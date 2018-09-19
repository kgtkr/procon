extern crate core;

use std::io::{self, Read};
use std::collections::HashSet;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split("\n")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|x| x.to_string())
        .collect::<HashSet<_>>();
    if list.len() == 3 { "Three" } else { "Four" }.to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "6
G W Y P Y W",
            "Four",
        ),
        (
            "9
G W W G P W P G G
",
            "Three",
        ),
        (
            "8
P Y W G Y W Y Y",
            "Four",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
