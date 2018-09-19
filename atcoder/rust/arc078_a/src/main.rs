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
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<_>>();
    let mut min = None;

    //0..iまでをすぬけが取る
    for i in 0..list.len() - 1 {
        let sunu = list[i];
        let kuma = list.last().unwrap() - list[i];
        let v = (sunu - kuma).abs();
        min = Some(match min {
            None => v,
            Some(m) => {
                if v < m {
                    v
                } else {
                    m
                }
            }
        });
    }
    min.unwrap().to_string()
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
1 2 3 4 5 6" => "1",
    test2: "2
10 -10" => "20",
}
