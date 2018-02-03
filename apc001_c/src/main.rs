extern crate core;

use std::io;

#[derive(PartialEq, Debug)]
enum Gender {
    Male,
    Female,
}

fn main() {
    let n = read_line().parse::<i32>().unwrap();
    println!("0");
    let g = read_g();
    is_ok(g, 0, n - 1);
}

fn is_ok(start_g: Gender, start: i32, end: i32) {
    let i = (start + end) / 2;
    let i = if i == start { end } else { i };
    println!("{}", i);
    let g = read_g();
    //左右どっちに矛盾があるか調べる
    //iが奇数なら異性、偶数なら同姓のはず
    let b = if (i - start) % 2 == 0 {
        start_g == g
    } else {
        start_g != g
    };

    //bがtrueなら右に矛盾。falseなら左に矛盾
    if b {
        is_ok(g, i, end);
    } else {
        is_ok(start_g, start, i);
    }
}

fn read_g() -> Gender {
    let input = read_line();
    match input.as_ref() {
        "Male" => Gender::Male,
        "Female" => Gender::Female,
        _ => std::process::exit(0),
    }
}

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}
