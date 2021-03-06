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
    input!(input=>(n:i64));
    let mut hashmap = HashMap::new();
    for i in 2..(n + 1) {
        prime_factor(i, &mut hashmap);
    }
    let mut res = 1;
    for i in hashmap.into_iter().map(|(_, x)| x + 1) {
        res = mul(res, i);
    }
    res.to_string()
}

use std::collections::HashMap;
//素因数分解
pub fn prime_factor(mut n: i64, res: &mut HashMap<i64, i64>) {
    let mut i = 2;
    while i * i <= n {
        while n % i == 0 {
            let v = match res.get(&i) {
                Some(v) => *v + 1,
                None => 1,
            };
            res.insert(i, v);
            n /= i;
        }
        i += 1;
    }
    if n != 1 {
        {
            let i = n;
            let v = match res.get(&i) {
                Some(v) => *v + 1,
                None => 1,
            };
            res.insert(i, v);
            n /= i;
        }
    }
}

fn mul(a: i64, b: i64) -> i64 {
    ((a % 1000000007) * (b % 1000000007)) % 1000000007
}

fn ncr(n: i64, r: i64) -> i64 {
    if r == 0 {
        1
    } else {
        mul((n - r + 1), ncr(n, r - 1)) / r
    }
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
    test1: "3" => "4",
    test2: "6" => "30",
    test3: "1000" => "972926972",
}
