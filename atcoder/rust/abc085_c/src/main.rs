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
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    let n = list[0];
    let n2 = list[1];

    //10000
    let mut x = 0;
    //5000
    let mut y = 0;
    //1000
    let mut z = n2 / 1000;

    loop {
        if x + y + z == n {
            return format!("{} {} {}", x, y, z);
        }

        if x + y + z < n {
            return "-1 -1 -1".to_string();
        }

        if z >= 5 && n + 4 <= x + y + z {
            z -= 5;
            y += 1;
        } else if y >= 2 {
            y -= 2;
            x += 1;
        } else {
            return "-1 -1 -1".to_string();
        }
    }
}

#[test]
fn test() {
    let tests = vec![
        ("9 45000", "0 9 0"),
        ("20 196000", "-1 -1 -1"),
        ("1000 1234000", "2 54 944"),
        ("2000 20000000", "2000 0 0"),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
