use std::io::{self, Read};
fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let vec: Vec<&str> = input.split_whitespace().collect();
    let a: f64 = vec[0].parse().unwrap();
    let b: f64 = vec[1].parse().unwrap();
    let x = (a + b) / 2.0;
    x.ceil().to_string()
}

use std::fs::File;

#[test]
fn test() {
    let tests = vec![("1 3", "2"), ("7 4", "6"), ("5 5", "5")];
    for (input, output) in tests {
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
