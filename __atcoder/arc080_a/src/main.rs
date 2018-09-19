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
        .map(|x| x.parse::<i32>().unwrap())
        .fold((0, 0, 0), |(a, b, c), x| {
            if x % 4 == 0 {
                (a + 1, b, c)
            } else if x % 2 == 0 {
                (a, b + 1, c)
            } else {
                (a, b, c + 1)
            }
        });
    let (a, b) = (list.0, list.2 + if list.1 == 0 { 0 } else { 1 });
    if a >= b - 1 { "Yes" } else { "No" }.to_string()
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
1 10 100" => "Yes",
    test2: "4
1 2 3 4" => "No",
    test3:"3
1 4 1"=>"Yes",
    test4:"2
1 1"=>"No",
test5:"6
2 7 1 8 2 8"=>"Yes",
}
