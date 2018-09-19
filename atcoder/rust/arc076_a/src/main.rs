extern crate core;

use std::io::{self, Read};
use std::cmp;
const N: i64 = 1000000007;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split_whitespace()
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let n = list[0];
    let m = list[1];
    if (n - m).abs() >= 2 {
        0
    } else {
        let min = fact_mod(cmp::min(n, m));
        if n == m {
            mul(mul(min, min), 2)
        } else {
            mul(mul(min, min), cmp::max(n, m))
        }
    }.to_string()
}

fn fact_mod(n: i64) -> i64 {
    let mut now = 1;
    for i in 1..n + 1 {
        now = mul(now, i);
    }
    now
}

fn mul(a: i64, b: i64) -> i64 {
    ((a % N) * (b % N)) % N
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
    test1: "2 2" => "8",
    test2: "3 2" => "12",
    test3: "1 8" => "0",
    test4: "100000 100000" => "530123477",
}
