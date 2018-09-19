extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    fn f(list: Vec<i64>, count: i32) -> i32 {
        if list.iter().all(|&x| x % 2 == 0) {
            f(list.iter().map(|x| x / 2).collect(), count + 1)
        } else {
            count
        }
    }

    f(
        input
            .split("\n")
            .nth(1)
            .unwrap()
            .split_whitespace()
            .map(|x| x.parse::<i64>().unwrap())
            .collect(),
        0,
    ).to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("3\n8 12 40", "2"),
        ("4\n5 6 8 10", "0"),
        (
            "6\n382253568 723152896 37802240 379425024 404894720 471526144",
            "8",
        ),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
