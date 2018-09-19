extern crate core;

use std::io::{self, Read};
use std::cmp::{max, min};

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
    if n == 1 && m == 1 {
        1
    } else if n == 1 || m == 1 {
        //1でない辺
        let l = max(n, m);
        l - 2
    } else {
        //両方2以上
        //囲まれている個数
        let a = (n - 2) * (m - 2);
        //辺の個数
        let b = (n - 2) * 2 + (m - 2) * 2;
        //角の個数
        let c = 4;
        a
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
    test1: "2 2" => "0",
    test2: "1 7" => "5",
    test3: "314 1592" => "496080",
}
