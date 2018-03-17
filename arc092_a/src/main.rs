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
        .split("\n")
        .map(|x| {
            let v = x.split_whitespace()
                .map(|x| x.parse::<i64>().unwrap())
                .collect::<Vec<_>>();
            (v[0], v[1])
        })
        .collect::<Vec<_>>();

    let reds = list.clone()
        .into_iter()
        .take(n)
        .enumerate()
        .collect::<Vec<_>>();
    let blues = list.clone()
        .into_iter()
        .skip(n)
        .enumerate()
        .collect::<Vec<_>>();
    let mut red_pairs = Vec::new();
    let mut blue_pairs = Vec::new();
    for _ in 0..n {
        red_pairs.push(Vec::new());
        blue_pairs.push(Vec::new());
    }
    //全ペアの個数
    let mut all_count = 0;
    for &(i, (a, b)) in &reds {
        for &(j, (c, d)) in &blues {
            if a < c && b < d {
                red_pairs[i].push(j);
                blue_pairs[j].push(i);
                all_count += 1;
            }
        }
    }

    let mut output = 0;

    while all_count != 0 {
        //rでペアになれる最小数(ただし0ではない)
        let r_min = red_pairs
            .clone()
            .into_iter()
            .map(|x| x.len())
            .filter(|&x| x != 0)
            .min()
            .unwrap();

        //(r,bのペアリスト)
        let r_min_list = red_pairs
            .clone()
            .into_iter()
            .enumerate()
            .filter(|&(i, ref x)| x.len() == r_min)
            .collect::<Vec<_>>();

        // ペア
        let (min_r, min_b, _) = r_min_list
            .into_iter()
            .map(|(i, bs)| {
                bs.clone()
                .into_iter()
                //(赤,青,個数)
                .map(|x| (i,x, blue_pairs[x].len()))
                .filter(|&(_,_,x)|x!=0)
                .min_by_key(|&(_,_,x)| x)
                .unwrap()
            })
            .min_by_key(|&(_, _, x)| x)
            .unwrap();

        output += 1;

        // 削除
        all_count -= red_pairs[min_r].len();
        all_count -= blue_pairs[min_b].len();

        red_pairs[min_r] = Vec::new();
        blue_pairs[min_b] = Vec::new();

        /*for vec in &blue_pairs{
            for x in 
        }*/
    }
    output.to_string()
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
    test1: "3 9" => "12",
    test2: "31 32" => "63",
    test3: "1 2" => "3",
    test4: "-1 2" => "1",
    test5: "10 1" => "11",
}
