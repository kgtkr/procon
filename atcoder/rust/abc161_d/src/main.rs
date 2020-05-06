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
    input!(input=>(k:usize));
    let mut count = 0;

    for n in 1.. {
        if let Some(result) = gen_n(n, &mut count, k, Vec::new()) {
            return result
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("");
        }
    }
    panic!();
}

// n桁のを生成
fn gen_n(n: usize, count: &mut usize, k: usize, prev: Vec<i64>) -> Option<Vec<i64>> {
    if n == prev.len() {
        *count += 1;

        if *count == k {
            Some(prev)
        } else {
            None
        }
    } else {
        match prev.last().clone() {
            None => {
                for i in 1..10 {
                    let mut cur = prev.clone();
                    cur.push(i);
                    if let Some(result) = gen_n(n, count, k, cur) {
                        return Some(result);
                    }
                }
            }
            Some(&last) => {
                for i in match last {
                    0 => vec![0, 1],
                    9 => vec![8, 9],
                    last => vec![last - 1, last, last + 1],
                } {
                    let mut cur = prev.clone();
                    cur.push(i);
                    if let Some(result) = gen_n(n, count, k, cur) {
                        return Some(result);
                    }
                }
            }
        }

        return None;
    }
}

macro_rules! tests {
    ($($name:ident: $input:expr=>$output:expr,)*) => {
        mod tests {
            $(                #[test]
                fn $name() {
                    assert_eq!($output.trim().to_string(),super::solve($input.trim().to_string()));
                }
            )*
        }
    }
}
tests! {
    test1: "15\n" => "23\n",
    test2: "1\n" => "1\n",
    test3: "13\n" => "21\n",
    test4: "100000\n" => "3234566667\n",
}
