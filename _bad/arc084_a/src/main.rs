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
        .split("\n")
        .skip(1)
        .map(|x| {
            x.split_whitespace()
                .map(|y| y.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    fn f(x: i32, vec: &Vec<Vec<i32>>, i: usize) -> i32 {
        *&vec[i]
            .iter()
            .filter(|&&y| x < y)
            .map(|&y| {
                if i == vec.len() - 1 {
                    1
                } else {
                    f(y, vec, i + 1)
                }
            })
            .sum::<i32>()
    }

    f(0, &list, 0).to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("2\n1 5\n2 4\n3 6", "3"),
        ("3\n1 1 1\n2 2 2\n3 3 3", "27"),
        (
            "6\n3 14 159 2 6 53\n58 9 79 323 84 6\n2643 383 2 79 50 288",
            "87",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
