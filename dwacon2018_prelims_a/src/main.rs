extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let l = input.chars().collect::<Vec<_>>();
    if l[0] == l[2] && l[1] == l[3] {
        "Yes"
    } else {
        "No"
    }.to_string()
}

#[test]
fn test() {
    let tests = vec![("2525", "Yes"), ("1881", "No")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
