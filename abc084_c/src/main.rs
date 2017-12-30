extern crate core;

use std::io::{self, Read};
use std::iter;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let list = input
        .split("\n")
        .skip(1)
        .map(|line| {
            let ll = line.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            //c,s,f
            (ll[0], ll[1], ll[2])
        })
        .collect::<Vec<_>>();
    let mut result: Vec<String> = Vec::new();
    for (i, _) in list.iter().enumerate() {
        let mut now_time = 0;
        for &(c, s, f) in list.iter().skip(i) {
            for k in (0..) {
                let t = s + k * f;
                if now_time <= t {
                    now_time = t + c;
                    break;
                }
            }
        }
        result.push(now_time.to_string());
    }
    result.join("\n") + "\n0"
}

#[test]
fn test() {
    let tests = vec![
        ("3\n6 5 1\n1 10 1", "12\n11\n0"),
        ("4\n12 24 6\n52 16 4\n99 2 2", "187\n167\n101\n0"),
        ("4\n12 13 1\n44 17 17\n66 4096 64", "4162\n4162\n4162\n0"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
