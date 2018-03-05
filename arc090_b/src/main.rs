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
        .parse::<i32>()
        .unwrap();
    let list = input
        .split("\n")
        .skip(1)
        .map(|x| {
            let v = x.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            (v[0] as usize, v[1] as usize, v[2])
        })
        .collect::<Vec<_>>();

    fn f((l, r, d): M, list: &Vec<M>) -> bool {
        let inside=list.iter()
            .filter(|&&(ref l2, ref r2, _)| l <= l2.clone() && r2.clone() <= r);

        //含まれ
        if r - l == 1 {
            true
        } else if {

        }
    }
    //iとi+1の距離
    for (l, r, d) in list {
        //含まれている
    }

    1.to_string()
}

#[test]
fn test() {
    let tests = vec![("3 9", "12"), ("31 32", "63")];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
