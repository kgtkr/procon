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
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    //500
    let a = list[0];
    //100
    let b = list[1];
    //50
    let c = list[2];
    let x = list[3];

    let mut n = 0;
    for i in 0..(a + 1) {
        for j in 0..(b + 1) {
            for k in 0..(c + 1) {
                if i * 500 + j * 100 + k * 50 == x {
                    n += 1;
                }
            }
        }
    }
    n.to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "2
2
2
100",
            "2",
        ),
        (
            "5
1
0
150",
            "0",
        ),
        (
            "30
40
50
6000",
            "213",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
