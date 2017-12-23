extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let list: Vec<i64> = input
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect();
    let x = list[0];
    let y = list[1];

    let mut vec: Vec<i64> = Vec::new();
    vec.push(x);
    for i in x..(y + 1) {
        let &last = vec.last().unwrap();
        if last < i && i % last == 0 {
            vec.push(i);
        }
    }

    vec.len().to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("3 20", "3"),
        ("25 100", "3"),
        ("314159265 358979323846264338", "31"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
