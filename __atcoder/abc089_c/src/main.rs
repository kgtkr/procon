extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let mut m = 0;
    let mut a = 0;
    let mut r = 0;
    let mut c = 0;
    let mut h = 0;
    for x in input.split("\n").skip(1).map(|x| x.to_string()) {
        if x.find("M") == Some(0) {
            m += 1;
        } else if x.find("A") == Some(0) {
            a += 1;
        } else if x.find("R") == Some(0) {
            r += 1;
        } else if x.find("C") == Some(0) {
            c += 1;
        } else if x.find("H") == Some(0) {
            h += 1;
        }
    }

    let mut list: Vec<i64> = vec![];
    if m != 0 {
        list.push(m);
    }
    if a != 0 {
        list.push(a);
    }
    if r != 0 {
        list.push(r);
    }
    if c != 0 {
        list.push(c);
    }
    if h != 0 {
        list.push(h);
    }

    //グループの数
    let n = list.len();
    if n == 3 {
        list[0] * list[1] * list[2]
    } else if n == 4 {
        list[0] * list[1] * list[2] + list[0] * list[1] * list[3] + list[0] * list[2] * list[3]
            + list[1] * list[2] * list[3]
    } else if n == 5 {
        list[0] * list[1] * list[2] + list[0] * list[1] * list[3] + list[0] * list[1] * list[4]
            + list[0] * list[2] * list[3] + list[0] * list[2] * list[4]
            + list[0] * list[3] * list[4] + list[1] * list[2] * list[3]
            + list[1] * list[2] * list[4] + list[1] * list[3] * list[4]
            + list[2] * list[3] * list[4]
    } else {
        0
    }.to_string()
}

#[test]
fn test() {
    let tests = vec![
        (
            "5
MASHIKE
RUMOI
OBIRA
HABORO
HOROKANAI",
            "2",
        ),
        (
            "4
ZZ
ZZZ
Z
ZZZZZZZZZZ",
            "0",
        ),
        (
            "5
CHOKUDAI
RNG
MAKOTO
AOKI
RINGO",
            "7",
        ),
    ];
    for (i, (input, output)) in tests.into_iter().enumerate() {
        println!("test:{}", i);
        assert_eq!(run(input.to_string()), output.to_string());
    }
}
