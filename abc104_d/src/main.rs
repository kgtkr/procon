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
        ($line:expr, $name:ident, $t:tt) => {
            let $name = value!($line, $t);
        };
    }

    macro_rules! values_def {
        ($lines:expr, $n:expr, $name:ident, $t:tt) => {
            let $name = {
                let mut vec = Vec::new();
                for i in 0..$n {
                    let mut next = $lines.next().unwrap().split_whitespace();
                    vec.push(value!(next, $t));
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
    input!(input=>(s:#));
    let mut list = s.chars().collect::<Vec<_>>();
    list.reverse();

    let q_count = list.clone().into_iter().filter(|&x| x == '?').count();
    let mut q_count_pow_mod = 1;
    for _ in 0..q_count {
        q_count_pow_mod = mul(q_count_pow_mod, 3);
    }

    let mut a = 0i64;
    let mut b = 0i64;
    let mut c = 0i64;
    for x in list {
        match x {
            'A' => a = add(a, b),
            'B' => b = add(b, c),
            'C' => c = add(c, q_count_pow_mod),
            '?' => {
                a = add(a, div(b, 3));
                b = add(b, div(c, 3));
                c = add(c, div(q_count_pow_mod, 3));
            }
            _ => unreachable!(),
        }
    }
    a.to_string()
}

const MOD: i64 = 1000000007;

fn power(x: i64, y: i64) -> i64 {
    if y == 0 {
        1
    } else if y == 1 {
        x % MOD
    } else if y % 2 == 0 {
        power(x, y / 2).pow(2) % MOD
    } else {
        power(x, y / 2).pow(2) * x % MOD
    }
}

fn div(a: i64, b: i64) -> i64 {
    mul(a, power(b, MOD - 2))
}

fn add(a: i64, b: i64) -> i64 {
    (a + b) % MOD
}

fn mul(a: i64, b: i64) -> i64 {
    ((a % MOD) * (b % MOD)) % MOD
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
    test1: "A??C" => "8",
    test2: "ABCBC" => "3",
    test3: "????C?????B??????A???????" => "979596887",
    test_add1:"A?C"=>"1",
}
