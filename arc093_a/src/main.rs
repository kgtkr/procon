extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn run(input: String) -> String {
    let n = input
        .split_whitespace()
        .nth(0)
        .unwrap()
        .parse::<usize>()
        .unwrap();
    let list = input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<i64>().unwrap())
        .collect::<Vec<_>>();

    //i-1からiまでの料金
    let ens = {
        let mut now = 0;
        let mut v = Vec::new();
        for &x in &list {
            v.push((x - now).abs());
            now = x;
        }

        v
    };

    //全部行った時の料金
    let ens_sum = {
        let mut now = 0;
        let mut sum = 0;
        for &x in &list {
            sum += (x - now).abs();
            now = x;
        }

        sum + now.abs()
    };

    let mut out = Vec::new();
    for i in 0..list.len() {
        out.push(
            if i == n - 1 {
                ens_sum - ens[i] - list[i].abs() + list[i - 1].abs()
            } else if i == 0 {
                ens_sum - ens[i] - ens[i + 1] + list[i + 1].abs()
            } else {
                //iに行かない
                ens_sum - ens[i] - ens[i + 1] + (list[i - 1] - list[i + 1]).abs()
            }.to_string(),
        );
    }
    out.join("\n").to_string()
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
    test1: "3\n3 5 -1" => "12\n8\n10",
    test2: "5\n1 1 1 2 0" => "4\n4\n4\n2\n4",
    test3: "6\n-679 -2409 -3258 3095 -3291 -4462" => "21630\n21630\n19932\n8924\n21630\n19288",
}
