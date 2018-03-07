extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

type M = (usize, usize, i32);

fn run(input: String) -> String {
    let n = input
        .split_whitespace()
        .nth(0)
        .unwrap()
        .parse::<usize>()
        .unwrap();
    let list = input
        .split("\n")
        .skip(1)
        .map(|x| {
            let v = x.split_whitespace()
                .map(|x| x.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            (v[0] - 1, v[1] - 1, v[2] as i64)
        })
        .collect::<Vec<_>>();

    1.to_string()
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
    test1: "3 3
1 2 1
2 3 1
1 3 2" => "Yes",
    test2: "3 3
1 2 1
2 3 1
1 3 5" => "No",
    test3: "4 3
2 1 1
2 3 5
3 4 2" => "Yes",
    test4: "10 3
8 7 100
7 9 100
9 8 100" => "No",
    test5: "100 0" => "Yes",
}
