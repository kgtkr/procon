extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let h = input
        .split("\n")
        .nth(0)
        .unwrap()
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse::<i64>()
        .unwrap();

    let list = input
        .split("\n")
        .skip(1)
        .map(|x| {
            let vec = x.split_whitespace().collect::<Vec<_>>();
            (
                vec[0].parse::<i64>().unwrap(),
                vec[1].parse::<i64>().unwrap(),
            )
        })
        .collect::<Vec<_>>();

    let sum = {
        let mut l2 = list.clone().iter().map(|&(_, x)| x).collect::<Vec<_>>();
        l2.sort();
        l2.reverse();
        for i in 1..(l2.len()) {
            let x = l2.iter().take(i).sum::<i64>();
            if x > h {
                //投げのみで倒せる
                return i.to_string();
            }
        }
        //投げのみで与えられるダメージ
        l2.iter().sum::<i64>()
    };

    //残り
    let diff = h - sum;

    //攻撃回数
    let c = ((diff as f64) / (list.clone().iter().map(|&(x, _)| x).max().unwrap() as f64)).ceil()
        as usize;

    (list.len() + c).to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("1 10\n3 5", "3"),
        ("2 10\n3 5\n2 6", "2"),
        (
            "4 1000000000\n1 1\n1 10000000\n1 30000000\n1 99999999",
            "860000004",
        ),
        ("5 500\n5 44\n28 83\n46 62\n31 79\n40 43", "9"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
