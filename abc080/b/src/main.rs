extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let n = input
        .split_whitespace()
        .nth(0)
        .unwrap()
        .parse::<i32>()
        .unwrap();
    let fx = n.to_string()
        .chars()
        .map(|y| y.to_string().parse::<i32>().unwrap())
        .sum::<i32>();

    if n % fx == 0 { "Yes" } else { "No" }.to_string()
}

#[test]
fn test() {
    let tests = vec![("12", "Yes"), ("57", "No"), ("148", "No")];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
