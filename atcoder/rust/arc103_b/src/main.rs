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
    input!(input=>(n:usize){n;list:(i64,i64)});
    let len_list = list
        .clone()
        .into_iter()
        .map(|(a, b)| a.abs() + b.abs())
        .collect::<Vec<_>>();
    //偶奇が一致しなければ-1
    for x in len_list.clone() {
        if x % 2 != len_list[0] % 2 {
            return "-1".to_string();
        }
    }

    //長さ
    let m = len_list.into_iter().max().unwrap();
    let mut res = Vec::new();
    for (x, y) in list {
        if x.abs() > 100 || y.abs() > 100 {
            //部分点
            panic!();
        }
        let mut s = String::new();
        //残りの長さ
        for _ in 0..x.abs() {
            s.push(if x > 0 { 'R' } else { 'L' });
        }

        for _ in 0..y.abs() {
            s.push(if y > 0 { 'U' } else { 'D' });
        }
        for i in 0..(m - x.abs() - y.abs()) {
            s.push(if i % 2 == 0 { 'R' } else { 'L' });
        }
        res.push(s);
    }

    format!(
        "{}\n{}\n{}",
        m,
        std::iter::repeat("1")
            .take(m as usize)
            .collect::<Vec<_>>()
            .join(" "),
        res.join("\n")
    ).to_string()
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
    test1: "3\n-1 0\n0 3\n2 -1" => "2\n1 2\nRL\nUU\nDR",
    test2: "5\n0 0\n1 0\n2 0\n3 0\n4 0" => "-1",
    test3: "2\n1 1\n1 1" => "2\n1 1\nRU\nUR",
    test4: "3\n-7 -3\n7 3\n-3 -7" => "5\n3 1 4 1 5\nLRDUL\nRDULR\nDULRD",
}
