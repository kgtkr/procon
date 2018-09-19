extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let vec: Vec<i64> = input
        .split("\n")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect();

    let mut map: HashMap<i64, i64> = HashMap::new();
    for x in vec {
        let insert = match map.get(&x) {
            Option::Some(y) => y + 1,
            Option::None => 1,
        };
        map.insert(x, insert);
    }

    map.iter()
        .map(|(&k, &v)| {
            if k == v {
                0
            } else if k < v {
                v - k
            } else {
                v
            }
        })
        .fold(0, |sum, i| sum + i)
        .to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("4\n3 3 3 3", "1"),
        ("5\n2 4 1 4 2", "2"),
        ("6\n1 2 2 3 3 3", "0"),
        ("1\n1000000000", "1"),
        ("8\n2 7 1 8 2 8 1 8", "5"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
