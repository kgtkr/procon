extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let (h, w, d) = {
        let list = input
            .split("\n")
            .nth(0)
            .unwrap()
            .split_whitespace()
            .map(|x| x.parse::<i32>().unwrap())
            .collect::<Vec<_>>();
        (list[0], list[1], list[2])
    };
    let a_map = {
        let map = input
            .split("\n")
            .skip(1)
            .take(h as usize)
            .map(|x| {
                x.split_whitespace()
                    .map(|x| x.parse::<i32>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let mut r = Vec::new();
        for _ in 0..(w * h) {
            r.push((0, 0));
        }
        for (x, a) in map.into_iter().enumerate() {
            for (y, b) in a.into_iter().enumerate() {
                r[b as usize - 1] = (x as i32, y as i32);
            }
        }
        r
    };

    //累積和
    let mut map_sum: Vec<(i32, (i32, i32))> = vec![];
    for (i, (x, y)) in a_map.into_iter().enumerate() {
        let old = i as i32 - d;
        let v = (
            if old < 0 {
                0
            } else {
                let (old_sum, (old_x, old_y)) = map_sum[old as usize];
                old_sum + (x - old_x).abs() + (y - old_y).abs()
            },
            (x, y),
        );
        map_sum.push(v);
    }
    let map_sum = map_sum.into_iter().map(|(x, _)| x).collect::<Vec<_>>();

    let q_list = input
        .split("\n")
        .skip((2 + h) as usize)
        .map(|x| {
            let list = x.split_whitespace()
                .map(|x| x.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
            (list[0], list[1])
        })
        .collect::<Vec<_>>();
    let mut s = Vec::new();
    for (l, r) in q_list {
        s.push((map_sum[r - 1] - map_sum[l - 1]).to_string());
    }
    s.join("\n").to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "3 3 2
1 4 3
2 5 7
8 9 6
1
4 8",
            "5",
        ),
        (
            "4 2 3
3 7
1 4
5 2
6 8
2
2 2
2 2",
            "0
0",
        ),
        (
            "5 5 4
13 25 7 15 17
16 22 20 2 9
14 11 12 1 19
10 6 23 8 18
3 21 5 24 4
3
13 13
2 10
13 13",
            "0
5
0",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
