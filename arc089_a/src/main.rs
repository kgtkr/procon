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
            let t = x.split_whitespace()
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            //t-x-y
            (t[0], t[1], t[2])
        })
        .collect::<Vec<_>>();

    let mut now_t = 0;
    let mut now_x = 0;
    let mut now_y = 0;
    for (next_t, next_x, next_y) in list {
        for _ in 0..(next_t - now_t) {
            if next_x > now_x {
                now_x += 1;
            } else if next_x < now_x {
                now_x -= 1;
            } else if next_y > now_y {
                now_y += 1;
            } else {
                now_y -= 1;
            }
            now_t += 1;
        }
        if (next_t, next_x, next_y) != (now_t, now_x, now_y) {
            return "No".to_string();
        }
    }
    return "Yes".to_string();
}

#[test]
fn test() {
    let tests = vec![
        ("2\n3 1 2\n6 1 1", "Yes"),
        ("1\n2 100 100", "No"),
        ("2\n5 1 1\n100 1 1", "No"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
