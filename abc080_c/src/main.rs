extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let n = input.split("\n").nth(0).unwrap().parse::<usize>().unwrap();
    let f = input
        .split("\n")
        .skip(1)
        .take(n)
        .map(|x| {
            x.split_whitespace()
                .map(|x| x.parse::<usize>().unwrap())
                .collect::<Vec<_>>()
        })
        .enumerate()
        .collect::<Vec<_>>();

    let p = input
        .split("\n")
        .skip(1 + n)
        .take(n)
        .map(|x| {
            x.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut max = None;
    //総あたり
    for i_0 in &[1, 0] {
        for i_1 in &[1, 0] {
            for i_2 in &[1, 0] {
                for i_3 in &[1, 0] {
                    for i_4 in &[1, 0] {
                        for i_5 in &[1, 0] {
                            for i_6 in &[1, 0] {
                                for i_7 in &[1, 0] {
                                    for i_8 in &[1, 0] {
                                        for i_9 in &[1, 0] {
                                            if !(*i_0 == 0 && *i_1 == 0 && *i_2 == 0 && *i_3 == 0
                                                && *i_4 == 0
                                                && *i_5 == 0
                                                && *i_6 == 0
                                                && *i_7 == 0
                                                && *i_8 == 0
                                                && *i_9 == 0)
                                            {
                                                let mut sum = 0;
                                                for &(ref i, ref x) in &f {
                                                    sum += p[*i][x[0] * i_0 + x[1] * i_1
                                                                     + x[2] * i_2
                                                                     + x[3] * i_3
                                                                     + x[4] * i_4
                                                                     + x[5] * i_5
                                                                     + x[6] * i_6
                                                                     + x[7] * i_7
                                                                     + x[8] * i_8
                                                                     + x[9] * i_9];
                                                }
                                                match max {
                                                    None => max = Some(sum),
                                                    Some(c) if sum > c => max = Some(sum),
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    max.unwrap().to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "1
1 1 0 1 0 0 0 1 0 1
3 4 5 6 7 8 9 -2 -3 4 -2",
            "8",
        ),
        (
            "2
1 1 1 1 1 0 0 0 0 0
0 0 0 0 0 1 1 1 1 1
0 -2 -2 -2 -2 -2 -1 -1 -1 -1 -1
0 -2 -2 -2 -2 -2 -1 -1 -1 -1 -1",
            "-2",
        ),
        (
            "3
1 1 1 1 1 1 0 0 1 1
0 1 0 1 1 1 1 0 1 0
1 0 1 1 0 1 0 1 0 1
-8 6 -2 -8 -8 4 8 7 -6 2 2
-9 2 0 1 7 -5 0 -2 -6 5 5
6 -6 7 -9 6 -5 8 0 -9 -7 -7",
            "23",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
