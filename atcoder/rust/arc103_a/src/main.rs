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
    input!(input=>(n:usize)(list:[usize]));
    let x = list[0];
    if list.clone().into_iter().all(|y| x == y) {
        return (n / 2).to_string();
    }
    let a = {
        let mut vec = Vec::new();
        vec.resize(100001, 0);
        for (i, x) in list.clone().into_iter().enumerate() {
            if i % 2 == 0 {
                vec[x] += 1;
            }
        }
        let mut vec = vec.into_iter().enumerate().collect::<Vec<_>>();
        vec.sort_by_key(|&(_, x)| x);
        vec.reverse();
        vec
    };

    let b = {
        let mut vec = Vec::new();
        vec.resize(100001, 0);
        for (i, x) in list.clone().into_iter().enumerate() {
            if i % 2 == 1 {
                vec[x] += 1;
            }
        }
        let mut vec = vec.into_iter().enumerate().collect::<Vec<_>>();
        vec.sort_by_key(|&(_, x)| x);
        vec.reverse();
        vec
    };

    if a[0].0 == b[0].0 {
        std::cmp::min(n - a[0].1 - b[1].1, n - a[1].1 - b[0].1)
    } else {
        n - a[0].1 - b[0].1
    }.to_string()
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
    test1: "4\n3 1 3 2" => "1",
    test2: "6\n105 119 105 119 105 119" => "0",
    test3: "4\n1 1 1 1" => "2",
}
