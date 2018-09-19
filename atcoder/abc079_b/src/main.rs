extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn l(i: i32, map: &mut HashMap<i32, i64>) -> i64 {
    match map.get(&i) {
        Option::Some(&x) => x,
        Option::None => {
            let x = l(i - 1, map) + l(i - 2, map);
            map.insert(i, x);
            x
        }
    }
}

fn lucas(i: i32) -> i64 {
    let mut map = HashMap::new();
    map.insert(0, 2);
    map.insert(1, 1);
    l(i, &mut map)
}

fn run(input: String) -> String {
    let i = input.trim().parse::<i32>().unwrap();

    lucas(i).to_string()
}

#[test]
fn test() {
    let tests = vec![("5", "11"), ("86", "939587134549734843")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
