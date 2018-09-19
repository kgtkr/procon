extern crate core;

use std::io::{self, Read};
use std::collections::BTreeSet;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let mut not_use = BTreeSet::new();
    for c in ('a' as u8)..('z' as u8 + 1) {
        not_use.insert(c as char);
    }

    for c in input.clone().chars() {
        not_use.remove(&c);
    }

    if input.len() != 26 {
        format!("{}{}", input, not_use.iter().next().unwrap())
    } else if input == "zyxwvutsrqponmlkjihgfedcba".to_string() {
        "-1".to_string()
    } else {
        for i in 1..27 {
            //iは削除する個数
            let base = &input[0..26 - i];
            let del = &input[26 - i..];
            let last = input.clone().chars().collect::<Vec<_>>()[26 - i];
            for c in del.clone().chars() {
                not_use.insert(c);
            }

            if let Some(c) = not_use.clone().into_iter().find(|&x| x > last) {
                return format!("{}{}", base, c);
            }
        }
        "-1".to_string()
    }
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!(super::run($input.to_string()), $output.to_string());
                }
            )*
        }
    }
}

tests! {
    test1: "atcoder" => "atcoderb",
    test2: "abc" => "abcd",
    test3: "zyxwvutsrqponmlkjihgfedcba" => "-1",
    test4: "abcdefghijklmnopqrstuvwzyx" => "abcdefghijklmnopqrstuvx",
    test_add1:"yzxwvutsrqponmlkjihgfedcba"=>"z",
}
