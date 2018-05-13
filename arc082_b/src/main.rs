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

fn swap(i: usize, list: &mut Vec<i64>) {
    let l = list[i] - 1;
    let r = list[i + 1] + 1;
    list[i + 1] = l;
    list[i] = r;
}

fn solve(input: String) -> String {
    input!(input=>(n:usize)(list:[i64]));
    let mut list = list.into_iter()
        .map(|x| x - 1)
        .enumerate()
        .map(|(a, b)| b - a as i64)
        .collect::<Vec<_>>();
    let mut count = 0;
    for i in 0..n {
        if list[i] == 0 {
            if i == 0 {
                swap(i, &mut list);
            } else if i == n - 1 {
                swap(i - 1, &mut list);
            } else {
                if list[i + 1] == 0 {
                    swap(i, &mut list);
                } else if list[i - 1] == 1 {
                    swap(i, &mut list);
                } else {
                    swap(i - 1, &mut list);
                }
            }
            count += 1;
        }
    }
    count.to_string()
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
    test1: "5\n1 4 3 5 2" => "2",
    test2: "2\n1 2" => "1",
    test3: "2\n2 1" => "0",
    test4: "9\n1 2 4 9 5 8 7 3 6" => "3",
}
