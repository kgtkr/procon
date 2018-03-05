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
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<_>>();
    let n = list[0];
    let m = list[1];
    ((1900 * m + 100 * (n - m)) * 2u32.pow(m)).to_string()
}

#[test]
fn test() {
    let tests = vec![("1 1", "3800"), ("10 2", "18400"), ("100 5", "608000")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
