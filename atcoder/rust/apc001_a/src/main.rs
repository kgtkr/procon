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
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let x = list[0];
    let y = list[1];
    if x % y == 0 {
        return "-1".to_string();
    }
    let mut i = 1;
    loop {
        let n = x * i;
        if n > 1000000000000000000 {
            return "-1".to_string();
        }
        if n % y != 0 {
            return n.to_string();
        }
        i += 1;
    }
}

#[test]
fn test() {
    let tests = vec![("8 6", "8"), ("3 3", "-1"), ("6 3", "-1"), ("3 6", "3")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
