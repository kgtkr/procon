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

fn solve(input: String) -> String {
    input!(input => (s: #));

    // 10^i mod 2019
    let mod2019 = (0..210000)
        .scan(1i64, |state, _| {
            let ret = *state;
            *state = (*state * 10) % 2019;
            Some(ret)
        })
        .collect::<Vec<_>>();

    s.chars()
        .map(|c| (c as u32 - '0' as u32) as i64)
        .rev()
        .enumerate()
        .scan(0, |state, (i, x)| {
            *state = (*state + x * mod2019[i]) % 2019;
            Some(*state)
        })
        .fold(
            {
                let mut v = Vec::with_capacity(2019);
                v.resize(2019, 0);
                v[0] = 1;
                v
            },
            |mut acc, x| {
                acc[x as usize] += 1;
                acc
            },
        )
        .into_iter()
        .filter(|&x| x != 0)
        .map(|x| x * (x - 1) / 2)
        .sum::<i64>()
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
    test1: "1817181712114\n" => "3\n",
    test2: "14282668646\n" => "2\n",
    test3: "2119\n" => "0\n",
}
