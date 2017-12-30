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
    let a = list[0].parse::<i32>().unwrap();
    let b = list[1].parse::<i32>().unwrap();
    let s = list[2];
    if s.len() != (a + b + 1) as usize {
        "No"
    } else {
        if s.chars().enumerate().all(|(i, c)| {
            if i == a as usize {
                c == '-'
            } else {
                c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6'
                    || c == '7' || c == '8' || c == '9'
            }
        }) {
            "Yes"
        } else {
            "No"
        }
    }.to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("3 4\n269-6650", "Yes"),
        ("1 1\n---", "No"),
        ("1 2\n7444", "No"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
