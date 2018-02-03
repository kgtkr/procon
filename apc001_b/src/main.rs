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
        .split("\n")
        .skip(1)
        .map(|x| {
            x.split_whitespace()
                .map(|x| x.parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut a = list[0].clone();
    let mut b = list[1].clone();

    let mut a_sum = a.iter().sum::<i64>();
    let mut b_sum = b.iter().sum::<i64>();

    loop {
        if a == b {
            return "Yes".to_string();
        }
        if a_sum >= b_sum {
            return "No".to_string();
        }

        let mut a_c = 0;
        let mut b_c = 0;
        for i in 0..a.len() {
            if a[i] < b[i] {
                let d = b[i] - a[i];
                let d = if d % 2 != 0 { d + 1 } else { d };

                a[i] += d;
                a_sum += d;
                a_c += d / 2;
            }

            if b[i] < a[i] {
                let d = a[i] - b[i];

                b[i] += d;
                b_sum += d;
                b_c += d;
            }
        }

        if a_c > b_c {
            let d = a_c - b_c;
            b[0] += d;
            b_sum += d;
        }

        if a_c < b_c {
            let d = (b_c - a_c) * 2;
            a[0] += d;
            a_sum += d;
        }
    }
}

#[test]
fn test() {
    let tests = vec![
        (
            "3
1 2 3
5 2 2",
            "Yes",
        ),
        (
            "5
3 1 4 1 5
2 7 1 8 2",
            "No",
        ),
        (
            "5
2 7 1 8 2
3 1 4 1 5",
            "No",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
