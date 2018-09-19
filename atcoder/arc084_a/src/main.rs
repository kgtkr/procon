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
            let mut v = x.split_whitespace()
                .map(|y| y.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            v.sort();
            v
        })
        .collect::<Vec<_>>();

    let mut sum = 0;
    //真ん中のパーツを決める
    for c in &list[1] {
        //上段の数
        let t = lower_bound(&list[0], 0, list[0].len(), c);
        let b = list[2].len() - upper_bound(&list[2], 0, list[2].len(), c);
        sum += t * b;
    }

    sum.to_string()
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!(super::run($input.to_string()), $output.to_string());
                }
            )*
        }
    }
}

tests! {
    test1: "2
1 5
2 4
3 6" => "3",
    test2: "3
1 1 1
2 2 2
3 3 3
" => "27",
    test3: "6
3 14 159 2 6 53
58 9 79 323 84 6
2643 383 2 79 50 288" => "87",
}

//vecは昇順ソート済み
//以上
fn lower_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] < val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] < val {
        last
    } else {
        first
    }
}

//より大きい
fn upper_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] <= val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] <= val {
        last
    } else {
        first
    }
}
