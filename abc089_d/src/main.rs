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

    let q_list = input
        .split("\n")
        .skip((2 + h) as usize)
        .map(|x| {
            let list = x.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            (list[0], list[1])
        })
        .collect::<Vec<_>>();
    let mut s = Vec::new();
    for (l, r) in q_list {
        let mut sum = 0;
        let mut now = l;
        while now != r {
            let next = now + d;
            let (now_x, now_y) = a_map[now as usize - 1];
            let (next_x, next_y) = a_map[next as usize - 1];
            sum += (now_x - next_x).abs() + (now_y - next_y).abs();
            now = next;
        }
        s.push(sum.to_string());
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
