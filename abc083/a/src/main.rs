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

    let a = list[0];
    let b = list[1];
    let c = list[2];
    let d = list[3];

    let l = a + b;
    let r = c + d;

    if r > l {
        "Right".to_string()
    } else if l > r {
        "Left".to_string()
    } else {
        "Balanced".to_string()
    }
}

#[test]
fn test() {
    let tests = vec![
        ("3 8 7 1", "Left"),
        ("3 4 5 2", "Balanced"),
        ("1 7 6 4", "Right"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
