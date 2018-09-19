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
    let mut list = input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i64>().unwrap())
        .fold(HashMap::new(), |mut map, x| {
            let v = map.get(&x).map(|i| i + 1).unwrap_or(1);
            map.insert(x, v);
            map
        })
        .into_iter()
        .flat_map(|(x, y)| {
            if y >= 4 {
                vec![x, x]
            } else if y >= 2 {
                vec![x]
            } else {
                vec![]
            }
        })
        .collect::<Vec<_>>();
    list.sort();
    list.reverse();
    if list.len() >= 2 {
        list[0] * list[1]
    } else {
        0
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
    test1: "6
3 1 2 4 2 1" => "2",
    test2: "4
1 2 3 4" => "0",
test3:"10
3 3 3 3 4 4 4 5 5 5"=>"20",
}
