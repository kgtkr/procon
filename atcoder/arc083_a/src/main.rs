extern crate core;

use std::io::{self, Read};
use std::collections::HashSet;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    let a = list[0] * 100;
    let b = list[1] * 100;
    let c = list[2];
    let d = list[3];
    let e = list[4];
    let f = list[5];

    //砂糖の組み合わせ
    let ss = {
        let mut set = HashSet::new();
        for i in 0..1500 + 1 {
            for j in 0..1500 + 1 {
                set.insert(i * c + j * d);
            }
        }
        let mut vec = set.into_iter().collect::<Vec<_>>();
        vec.sort();
        vec
    };

    //パーセント/砂糖水/砂糖
    let mut max = (0.0, 0, 0);

    for i_1 in 0..30 + 1 {
        for i_2 in 0..30 + 1 {
            for &s in &ss {
                let w = i_1 * a + i_2 * b;
                //溶け残ってる||質量オーバー
                if s > w * e / 100 || s + w > f {
                    break;
                }

                //濃度
                let p = if w + s == 0 {
                    0.0
                } else {
                    100.0 * s as f64 / (s + w) as f64
                };

                //濃度がより高ければ
                if p > max.0 {
                    max = (p, w + s, s);
                }
            }
        }
    }

    if max.1 == 0 && max.2 == 0 {
        max.1 = a;
    }

    format!("{} {}", max.1, max.2).to_string()
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
    test1: "1 2 10 20 15 200" => "110 10",
    test2: "1 2 1 2 100 1000" => "400 200",
    test3:"17 19 22 26 55 2802"=>"2634 934",
}
