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
    input!(input=>(n:usize)(s:#));
    let mut list1 = s.clone()
        .chars()
        .map(|x| if x == 'W' { 1 } else { 0 })
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<_>>();

    let mut list2 = s.chars()
        .map(|x| if x == 'E' { 1 } else { 0 })
        .collect::<Vec<_>>();
    list2.reverse();
    list2 = list2
        .into_iter()
        .scan(0, |state, x| {
            *state = *state + x;
            Some(*state)
        })
        .collect::<Vec<_>>();
    list2.reverse();

    (list1
        .into_iter()
        .zip(list2.into_iter())
        .map(|(a, b)| a + b)
        .fold(10000000, |a, b| std::cmp::min(a, b)) - 1)
        .to_string()
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
    test1: "5\nWEEWW" => "1",
    test2: "12\nWEWEWEEEWWWE" => "4",
    test3: "8\nWWWWWEEE" => "3",
}
