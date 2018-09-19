extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let mut iter = input.split("\n").map(|x| {
        let v = x.split_whitespace()
            .map(|x| x.parse::<i64>().unwrap())
            .collect::<Vec<_>>();
        (v[0], v[1])
    });
    let (_, k) = iter.next().unwrap();
    let mut list = iter.collect::<Vec<_>>();
    list.sort_by_key(|&(x, _)| x);
    list.into_iter()
        .scan((0, 0), |state, (a, b)| {
            *state = (a, (*state).1 + b);
            Some(*state)
        })
        .find(|&(a, b)| b >= k)
        .map(|(x, _)| x)
        .unwrap()
        .to_string()
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
    test1: "3 4
1 1
2 2
3 3" => "3",
    test2: "10 500000
1 100000
1 100000
1 100000
1 100000
1 100000
100000 100000
100000 100000
100000 100000
100000 100000
100000 100000" => "1",
}
