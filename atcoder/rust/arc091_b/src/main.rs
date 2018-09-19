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
    let k = list[1];
    let mut sum = 0;
    //bの座標
    for i in 1..n + 1 {
        //i:周期
        //p:何周
        //q:余りの周、余りの最大値
        let p = n / i;
        let q = n % i;

        //1周にk以上が何個あるか
        let c = max(0, i - k);

        //余り(1〜q)にk以上が何個あるか
        let c2 = if k == 0 { q } else { max(0, q - k + 1) };

        sum += p * c + c2;
    }
    sum.to_string()
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
    test1: "5 2" => "7",
    test2:"10 0"=>"100",
    test3:"31415 9265"=>"287927211",
}
