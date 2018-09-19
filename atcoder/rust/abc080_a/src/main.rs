extern crate core;

use std::io::{self, Read};
use std::cmp;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let list: Vec<i32> = input
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();
    let n = list[0];
    let a = list[1];
    let b = list[2];

    cmp::min(n * a, b).to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("7 17 120", "119"),
        ("5 20 100", "100"),
        ("6 18 100", "100"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
