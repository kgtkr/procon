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
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();
    let a = list[0];
    let b = list[1];
    let mut count = 0;
    for i in a..b + 1 {
        let x = i.to_string().chars().collect::<Vec<_>>();
        let mut y = x.clone();
        y.reverse();
        if (x == y) {
            count += 1;
        }
    }
    count.to_string()
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
    test1: "11009 11332" => "4",
    test2: "31415 92653" => "612",
}
