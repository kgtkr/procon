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

#[derive(Clone, Copy, Debug)]
enum Cmd {
    F(i64),
    T(i64),
}

fn solve(input: String) -> String {
    input!(input=>(s:#)(target:(i64,i64)));
    let (xlist, ylist) = {
        let list = {
            let mut vec = Vec::new();
            let mut old = 'X';
            let mut count = 0;
            for x in s.chars() {
                if old == x {
                    count += 1;
                } else {
                    if count != 0 {
                        if old == 'F' {
                            vec.push(Cmd::F(count));
                        } else {
                            vec.push(Cmd::T(count));
                        }
                    }
                    count = 1;
                }
                old = x;
            }
            if count != 0 {
                if old == 'F' {
                    vec.push(Cmd::F(count));
                } else {
                    vec.push(Cmd::T(count));
                }
            }
            vec
        };

        let mut xlist = Vec::new();
        let mut ylist = Vec::new();
        let mut is_x = true;

        for x in list {
            if let Cmd::F(x) = x {
                if is_x {
                    xlist.push(x);
                } else {
                    ylist.push(x);
                }
            } else if let Cmd::T(x) = x {
                if x % 2 == 1 {
                    is_x = !is_x;
                }
            }
        }
        (xlist, ylist)
    };

    let size = std::cmp::max(xlist.len(), ylist.len());
    let mut dp = Vec::with_capacity(size);
    dp.resize(size, {
        let mut v = Vec::with_capacity(16000);
        v.resize(16000, None);
        v
    });

    let xb = f(&xlist, target.0, 0, 0, &mut dp.clone());
    let yb = f(&ylist, target.1, 0, 0, &mut dp);

    if xb && yb { "Yes" } else { "No" }.to_string()
}

fn f(list: &Vec<i64>, target: i64, i: usize, point: i64, dp: &mut Vec<Vec<Option<bool>>>) -> bool {
    let index = (point + 8000) as usize;
    if list.len() == i {
        target == point
    } else if let Some(b) = dp[i][index] {
        b
    } else {
        let b = f(list, target, i + 1, point + list[i], dp)
            || f(list, target, i + 1, point - list[i], dp);
        dp[i][index] = Some(b);
        b
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
    test1: "FTFFTFFF\n4 2" => "Yes",
    test2: "FTFFTFFF\n-2 -2" => "Yes",
    test3: "FF\n1 0" => "No",
    test4: "TF\n1 0" => "No",
    test5: "FFTTFF\n0 0" => "Yes",
    test6: "TTTT\n1 0" => "No",
}
