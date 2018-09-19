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
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let n = list[0].len();
    let mut max = 0;

    //何回目で下に行くか
    for i in 0..n {
        let c = list[0].iter().take(i + 1).sum::<i32>() + list[1].iter().skip(i).sum::<i32>();
        if c > max {
            max = c;
        }
    }
    max.to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "5
3 2 2 4 1
1 2 2 2 1",
            "14",
        ),
        (
            "4
1 1 1 1
1 1 1 1",
            "5",
        ),
        (
            "7
3 3 4 5 4 5 3
5 3 4 4 2 3 2",
            "29",
        ),
        (
            "1
2
3",
            "5",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
