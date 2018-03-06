extern crate core;

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let n = input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i32>().unwrap())
        .fold(HashMap::new(), |mut map, x| {
            let v = map.get(&x).map(|i| i + 1).unwrap_or(1);
            map.insert(x, v);
            map
        })
        .into_iter()
        .filter(|&(_, x)| x % 2 != 0)
        .count();
    n.to_string()
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
6
2
6" => "1",
    test2: "4
2
5
5
2
" => "0",
test3:"6
12
22
16
22
18
12"=>"2",
}
