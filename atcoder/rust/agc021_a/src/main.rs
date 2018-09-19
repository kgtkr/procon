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
        .chars()
        .map(|x| x.to_string().parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    let a = (list[0] - 1) + 9 * ((list.len() as i32) - 1);
    let b = list.into_iter().sum::<i32>();
    if a > b { a } else { b }.to_string()
}

#[test]
fn test() {
    let tests = vec![("100", "18"), ("9995", "35"), ("3141592653589793", "137")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
