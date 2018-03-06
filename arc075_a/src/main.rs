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
        .skip(1)
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let sum = list.clone().into_iter().sum::<i64>();
    if sum % 10 == 0 {
        let min = list.into_iter().filter(|x| x % 10 != 0).fold(None, |c, x| {
            Some(match c {
                None => x,
                Some(c) => {
                    if x < c {
                        x
                    } else {
                        c
                    }
                }
            })
        });
        match min {
            Some(min) => sum - min,
            None => 0,
        }
    } else {
        sum
    }.to_string()
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
    test1: "3
5
10
15" => "25",
    test2: "3
10
10
15" => "35",
    test3: "3
10
20
30
" => "0",
}
