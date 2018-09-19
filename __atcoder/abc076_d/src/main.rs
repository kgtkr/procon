extern crate core;

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = run(input.trim().to_string());
    println!("{}", output);
}

fn max_v(start: f64, end: f64, v: f64, now: f64) -> f64 {
    if now < start {
        let d = start - now;
        v + d
    } else if end < now {
        let d = now - end;
        v + d
    } else {
        v
    }
}

fn run(input: String) -> String {
    let mut iter = input.split("\n");
    iter.next();
    let ts = iter.next()
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse::<f64>().unwrap())
        .collect::<Vec<_>>();

    let vs = iter.next()
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse::<f64>().unwrap())
        .collect::<Vec<_>>();

    //(t,v)
    let mut list = ts.clone().into_iter().zip(vs).collect::<Vec<_>>();

    //(start,end,v)
    let mut vec = Vec::new();
    let mut time_sum = 0.0;
    for (t, v) in list {
        vec.push((time_sum, time_sum + t, v));
        time_sum += t;
    }
    vec.push((0.0, 0.0, 0.0));
    vec.push((time_sum, time_sum, 0.0));

    //0.5sごとの瞬間の速度
    let mut v_list = Vec::new();
    for t in 0..time_sum.round() as i32 + 1 {
        for t in vec![t as f64, t as f64 + 0.5] {
            let min = vec.clone()
                .into_iter()
                .map(|x| max_v(x.0, x.1, x.2, t))
                .fold(0.0 / 0.0, f64::min);
            v_list.push(min);
        }
    }

    //0.5sごとの平均の速度
    let mut v_list2 = Vec::new();
    for i in 0..v_list.len() - 1 {
        v_list2.push((v_list[i] + v_list[i + 1]) / 2.0);
    }

    //平均の速度の合計を2で割る
    ((v_list2.into_iter().sum::<f64>() / 2.0) - 0.125).to_string()
}

fn round025(x: f64) -> f64 {
    (x * 4.0).round() / 4.0
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
    test1: "1
100
30" => "2100",
    test2: "2
60 50
34 38" => "2632",
    test3: "3
12 14 2
6 2 7" => "76",
    test4: "1
9
10" => "20.25",
    test5: "10
64 55 27 35 76 119 7 18 49 100
29 19 31 39 27 48 41 87 55 70" => "20291",
}
