extern crate core;

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
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    let x = list[0];
    let a = list[1];
    let b = list[2];
    ((x - a) % b).to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "1234
150
100",
            "84",
        ),
        (
            "1000
108
108",
            "28",
        ),
        (
            "579
123
456",
            "0",
        ),
        (
            "7477
549
593",
            "405",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
