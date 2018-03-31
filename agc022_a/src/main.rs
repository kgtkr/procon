extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let s = input.chars().map(|x| (x as usize) - 97).collect::<Vec<_>>();
    if s.len() != 26 {
        //使われてない文字で一番小さい文字を追加
        //使われてるかのフラグ
        let mut is_use = Vec::new();
        is_use.resize(26, false);
        for &x in &s {
            is_use[x] = true;
        }
        let add = is_use
            .into_iter()
            .enumerate()
            .find(|&(_, b)| !b)
            .map(|(i, _)| (i + 97) as u8 as char)
            .unwrap();
        format!("{}{}", input, add)
    } else if input == "zyxwvutsrqponmlkjihgfedcba".to_string() {
        "-1".to_string()
    } else {
        let mut is_use = Vec::new();
        is_use.resize(27, true);

        //後ろから見ていって一個大きいの入れれるか
        let mut change = 0;
        for x in {
            let mut s = s.clone();
            s.reverse();
            s
        } {
            if x != 25 && !is_use[x + 1] {
                change = x;
                break;
            }
            is_use[x] = false;
        }

        let mut s = s.into_iter()
            .take_while(|&x| x != change)
            .collect::<Vec<_>>();
        s.push(change + 1);

        s.into_iter()
            .map(|x| (x + 97) as u8 as char)
            .collect::<String>()
    }
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
    test1: "atcoder" => "atcoderb",
    test2: "abc" => "abcd",
    test3: "zyxwvutsrqponmlkjihgfedcba" => "-1",
    test4: "abcdefghijklmnopqrstuvwzyx" => "abcdefghijklmnopqrstuvx",
}
