extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let mut iter = input.split_whitespace().map(|x| x.parse::<i64>().unwrap());
    let n = iter.next().unwrap();
    let d = iter.next().unwrap();
    let x = iter.next().unwrap();
    let list = iter.collect::<Vec<_>>();
    let mut sum = 0;
    for a in list {
        for n in 0.. {
            if n * a + 1 > d {
                break;
            } else {
                sum += 1;
            }
        }
    }
    (x + sum).to_string()
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
    test1: "3\n7 1\n2\n5\n10" => "8",
    test2: "2\n8 20\n1\n10" => "29",
    test3: "5\n30 44\n26\n18\n81\n18\n6" => "56",
}
