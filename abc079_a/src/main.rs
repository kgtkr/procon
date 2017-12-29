extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.chars().collect::<Vec<char>>();

    if list[0] == list[1] && list[0] == list[2] || list[3] == list[2] && list[3] == list[1] {
        "Yes"
    } else {
        "No"
    }.to_string()
}

#[test]
fn test() {
    let tests = vec![("1118", "Yes"), ("7777", "Yes"), ("1234", "No")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
