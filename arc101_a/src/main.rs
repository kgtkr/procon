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
  //単一値
  ($line:expr,$t:ty) => {
    $line.next().unwrap().parse::<$t>().unwrap()
  };
  //インデックス(-1)
  ($line:expr,@) => {
    $line.next().unwrap().parse::<usize>().unwrap()-1
  };
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let output = solve(input.trim().to_string());
    println!("{}", output);
}

fn solve(input: String) -> String {
    input!(input=>(n:usize k:usize)(list:[i64]));
    let mut k = k;
    let mut sei = Vec::new();
    let mut hu = Vec::new();
    for x in list {
        if x > 0 {
            sei.push(x);
        } else if x < 0 {
            hu.push(-x);
        } else {
            k -= 1;
        }
    }

    if k == 0 {
        return 0.to_string();
    }

    hu.reverse();

    let mut min = <i64>::max_value();
    //正方向にb
    for a in 0..k + 1 {
        let b = k - a;

        if sei.len() >= a && hu.len() >= b {
            let a_sum = if a == 0 { 0 } else { sei[a - 1] };
            let b_sum = if b == 0 { 0 } else { hu[b - 1] };

            /*let c_sum = if a == 0 || b == 0 {
                0
            } else {
                std::cmp::min(sei[a - 1], hu[b - 1])
            };*/

            min = std::cmp::min(min, std::cmp::min(a_sum * 2 + b_sum, a_sum + b_sum * 2));
        }
    }
    min.to_string()
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
    test1: "5 3\n-30 -10 10 20 50" => "40",
    test2: "3 2\n10 20 30" => "20",
    test3: "1 1\n0" => "0",
    test4: "8 5\n-9 -7 -4 -3 1 2 3 4" => "10",
}
