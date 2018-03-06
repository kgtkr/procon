extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<u64>().unwrap())
        .fold(None, |a, b| {
            Some(match a {
                None => b,
                Some(a) => lcm(a, b),
            })
        })
        .unwrap()
        .to_string()
}

fn gcd(a: u64, b: u64) -> u64 {
    let r = a % b;
    if r == 0 {
        b
    } else {
        gcd(b, r)
    }
}

fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
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
    test1: "2
2
3" => "6",
    test2: "5
2
5
10
1000000000000000000
1000000000000000000" => "1000000000000000000",
}
