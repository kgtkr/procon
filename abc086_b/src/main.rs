extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.split_whitespace().collect::<Vec<_>>();
    let a = list[0];
    let b = list[1];
    let x = format!("{}{}", a, b).parse::<i32>().unwrap();
    let y = {
        let n = (x as f64).sqrt().floor() as i32;
        n * n
    };
    if x == y { "Yes" } else { "No" }.to_string()
}

#[test]
fn test() {
    let tests = vec![("1 21", "Yes"), ("100 100", "No"), ("12 10", "No")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
