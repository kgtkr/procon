extern crate core;

use std::io::{self, Read};

#[macro_use]
mod parser {
    macro_rules! input {
    ($s:expr=>$($t:tt)*) => {
        let mut lines=$s.split("\n");
        $(
            line_parse!(lines,$t);
        )*
    };
    }

    macro_rules! line_parse {
    ($lines:expr,($($name:ident:$t:tt)*)) => {
        let mut line=$lines.next().unwrap().split_whitespace();
        $(value_def!(line,$name,$t);)*
    };

    //複数行
    ($lines:expr,{$n:expr;$name:ident:$t:tt}) => {
        values_def!($lines,$n,$name,$t);
    };
    }

    macro_rules! value_def {
    ($line:expr,$name:ident,$t:tt) => {
        let $name=value!($line,$t);
    };
    }

    macro_rules! values_def {
    ($lines:expr,$n:expr,$name:ident,$t:tt) => {
        let $name={
        let mut vec=Vec::new();
        for i in 0..$n{
            let mut next=$lines.next().unwrap().split_whitespace();
            vec.push(value!(next,$t));
        }
        vec
        };
    };
    }

    macro_rules! value {
    //配列
    ($line:expr,[$t:tt]) => {
        $line.map(|x|{
        let mut iter=::std::iter::once(x);
        value!(iter,$t)
        }).collect::<Vec<_>>()
    };
    //タプル
    ($line:expr,($($t:tt),*)) => {
        ($(value!($line,$t),)*)
    };
    //文字列
    ($line:expr,#) => {
        $line.next().unwrap()
    };
    //単一値
    ($line:expr,$t:ty) => {
        $line.next().unwrap().parse::<$t>().unwrap()
    };
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize c:i64){n;list:(i64,i64)});
    let mut max = 0;

    //時計回りにi、向き変えてj
    for i in 0..n + 1 {
        //時計回り分
        let a = if i != 0 {
            list.clone()
                .into_iter()
                .take(i)
                .map(|(_, v)| v)
                .sum::<i64>()
        } else {
            0
        };

        for j in 0..n - i + 1 {
            //反時計回り分
            let b = if j != 0 {
                list.clone()
                    .into_iter()
                    .skip(n - j)
                    .map(|(_, v)| v)
                    .sum::<i64>()
            } else {
                0
            };

            let cost1 = if i != 0 { list[i - 1].0 } else { 0 };
            let cost2 = if j != 0 { c - list[n - j].0 } else { 0 };
            let cost = std::cmp::min(cost1 * 2 + cost2, cost1 + cost2 * 2);
            let res = a + b - cost;
            if max < res {
                max = res
            }
        }
    }
    max.to_string()
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(
                #[test]
                fn $name() {
                    assert_eq!($output.trim().to_string(),super::solve($input.trim().to_string()));
                }
            )*
        }
    }
}

tests! {
    test1: "3 20\n2 80\n9 120\n16 1" => "191",
    test2: "3 20\n2 80\n9 1\n16 120" => "192",
    test3: "1 100000000000000\n50000000000000 1" => "0",
    test4: "15 10000000000\n400000000 1000000000\n800000000 1000000000\n1900000000 1000000000\n2400000000 1000000000\n2900000000 1000000000\n3300000000 1000000000\n3700000000 1000000000\n3800000000 1000000000\n4000000000 1000000000\n4100000000 1000000000\n5200000000 1000000000\n6600000000 1000000000\n8000000000 1000000000\n9300000000 1000000000\n9700000000 1000000000" => "6500000000",
}
