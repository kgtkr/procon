extern crate core;

use std::io::{self, Read};

#[macro_export]
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
  //インデックス(-1)
  ($line:expr,@) => {
    $line.next().unwrap().parse::<usize>().unwrap()-1
  };
  //単一値
  ($line:expr,$t:ty) => {
    $line.next().unwrap().parse::<$t>().unwrap()
  };
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

/*
(1 ^ 2) + (1 ^ 3) + (1 ^ 4) = 10

(0001 ^ 0010) + (0001 ^ 0011) + (0001 ^ 0100) = 1010

(0010 + 0011 + 0100) = 0121 = 1001
0121を0001で
0の時1の数、1の時0の数

*/

fn solve(input: String) -> String {
    input!(input=>(n:usize)(list:[u64]));
    let mut list = list.into_iter().map(u64_to_bits).collect::<Vec<_>>();
    list.reverse();
    let mut sum_list = sum_list(&list);
    list.reverse();
    sum_list.reverse();

    let mut res = 0;

    for i in 0..list.len() - 1 {
        let x = sum_list[i + 1];
        let mask = list[i];
        let mut masked = [0; 64];
        for j in 0..masked.len() {
            masked[j] = if mask[j] == 0 {
                x[j]
            } else {
                (n - (i + 1)) as u64 - x[j]
            };
        }
        res = add(res, bits_to_u64(masked));
    }
    res.to_string()
}

fn sum_list(v: &Vec<[u64; 64]>) -> Vec<[u64; 64]> {
    v.iter()
        .scan([0; 64], |state, x| {
            *state = add_bits(*state, *x);
            Some(*state)
        })
        .collect()
}

const MOD: u64 = 1000000007;

fn add_bits(a: [u64; 64], b: [u64; 64]) -> [u64; 64] {
    let mut res = [0; 64];
    for i in 0..res.len() {
        res[i] = a[i] + b[i];
    }
    res
}

fn add(a: u64, b: u64) -> u64 {
    (a + b) % MOD
}

fn mul(a: u64, b: u64) -> u64 {
    ((a % MOD) * (b % MOD)) % MOD
}

fn u64_to_bits(x: u64) -> [u64; 64] {
    let mut res = [0; 64];
    for i in 0..res.len() {
        res[i] = (x & (0b00000001 << (res.len() - (i + 1)))) >> (res.len() - (i + 1));
    }
    res
}

fn bits_to_u64(bits: [u64; 64]) -> u64 {
    let mut res = 0;
    for i in 0..bits.len() {
        res = add(res, mul(bits[i], 0b00000001 << (bits.len() - (i + 1))));
    }
    res
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
    test1: "3\n1 2 3\n" => "6\n",
    test2: "10\n3 1 4 1 5 9 2 6 5 3\n" => "237\n",
    test3: "10\n3 14 159 2653 58979 323846 2643383 27950288 419716939 9375105820\n" => "103715602\n",
}
