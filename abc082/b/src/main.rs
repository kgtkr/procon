extern crate core;

use std::io::{self, Read};
use core::cmp::Ordering;
fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input);
    println!("{}", output);
}

fn run(input: String) -> String {
    let vec: Vec<&str> = input.split_whitespace().collect();
    let s = {
        let mut v = vec[0].chars().collect::<Vec<_>>();
        v.sort();
        v
    };
    let t = {
        let mut v = vec[1].chars().collect::<Vec<_>>();
        v.sort();
        v.reverse();
        v
    };
    match s.cmp(&t) {
        Ordering::Less => "Yes",
        _ => "No",
    }.to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("yx\naxy", "Yes"),
        ("ratcode\natlas", "Yes"),
        ("cd\nabc", "No"),
        ("w\nww", "Yes"),
        ("zzz\nzzz", "No"),
    ];
    for (i, &(input, output)) in tests.iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
