extern crate core;

use std::io::{self, Read};

#[derive(Debug, Clone, PartialEq)]
enum Color {
    W,
    B,
}

#[derive(Debug, Clone, PartialEq)]
struct Data {
    x: i64,
    y: i64,
    c: Color,
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let k = input
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse::<i64>()
        .unwrap();
    let list = input
        .split("\n")
        .skip(1)
        .map(|x| {
            let l = x.split_whitespace().collect::<Vec<_>>();
            Data {
                x: l[0].parse::<i64>().unwrap(),
                y: l[1].parse::<i64>().unwrap(),
                c: if l[2] == "W" { Color::W } else { Color::B },
            }
        })
        .collect::<Vec<_>>();

    let mut max = 0;
    //x,yをずらす
    for dx in 0..(k * 2) {
        for dy in 0..(k * 2) {
            let count = list.iter()
                .filter(|&d| {
                    let xm = (d.x + dx) % (k * 2) >= k;
                    let ym = (d.y + dy) % (k * 2) >= k;
                    //色
                    let m = xm == ym;
                    d.c == if m { Color::W } else { Color::B }
                })
                .count();

            if max < count {
                max = count;
            }
        }
    }

    max.to_string()
}

#[test]
fn test() {
    let tests = vec![
        ("4 3\n0 1 W\n1 2 W\n5 3 B\n5 4 B", "4"),
        ("2 1000\n0 0 B\n0 1 W", "2"),
        ("6 2\n1 2 B\n2 1 W\n2 2 B\n1 0 B\n0 6 W\n4 5 W", "4"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
