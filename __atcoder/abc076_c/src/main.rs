extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input.split_whitespace().collect::<Vec<_>>();
    let mut s = list[0].chars().collect::<Vec<_>>();
    let mut t = list[1].chars().collect::<Vec<_>>();
    if s.len() < t.len() {
        "UNRESTORABLE".to_string()
    } else {
        //どこに入るか前から順番に試していく
        let mut index = None;
        for i in 0..s.len() - t.len() + 1 {
            if (0..t.len()).all(|j| s[i + j] == t[j] || s[i + j] == '?') {
                index = Some(i)
            }
        }
        match index {
            Some(index) => {
                //sをtで置き換え
                for i in 0..t.len() {
                    s[index + i] = t[i];
                }
                //?をaで置き換え
                s.into_iter()
                    .map(|x| if x == '?' { 'a' } else { x })
                    .collect::<String>()
            }
            None => "UNRESTORABLE".to_string(),
        }
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
    test1: "?tc????
coder" => "atcoder",
    test2: "??p??d??
abc" => "UNRESTORABLE",
}
