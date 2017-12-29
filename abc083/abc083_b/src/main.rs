extern crate core;

use std::io::{self, Read};

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

    (1..(n+1))
        .map(|x| {
            (
                x,
                x.to_string()
                    .chars()
                    .map(|y| y.to_string().parse::<i32>().unwrap())
                    .fold(0, |sum, i| sum + &i),
            )
        })
        .filter(|&(_, x)| a <= x && x <= b)
        .map(|(i, _)| i)
        .fold(0, |sum, i| sum + &i)
        .to_string()
}

#[test]
fn test() {
    let tests = vec![("20 2 5", "84"), ("10 1 2", "13"), ("100 4 16", "4554")];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
