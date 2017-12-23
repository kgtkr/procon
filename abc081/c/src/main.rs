extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn f(vec: &mut Vec<(&i32, &i32)>, k: i32, count: i32) -> i32 {
    if vec.len() as i32 <= k {
        count
    } else {
        let (_, x) = vec.pop().unwrap();
        f(vec, k, count + x)
    }
}

fn run(input: String) -> String {
    let list: Vec<i32> = input
        .split("\n")
        .nth(1)
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();

    let k = input
        .split("\n")
        .nth(0)
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse::<i32>()
        .unwrap();

    let mut map: HashMap<i32, i32> = HashMap::new();
    for x in list {
        let insert = match map.get(&x) {
            Option::Some(y) => y + 1,
            Option::None => 1,
        };
        map.insert(x, insert);
    }

    let mut vec: Vec<(&i32, &i32)> = map.iter().collect();
    vec.sort_by(|&(_, a), &(_, b)| b.cmp(&a));

    f(&mut vec, k, 0).to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("5 2\n1 1 2 2 5", "1"),
        ("4 4\n1 1 2 2", "0"),
        ("10 3\n5 1 3 2 4 1 1 2 3 4", "3"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
