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
    let mut list = list;
    list.sort();
    let mut res = 0;
    for i in 1..n - 1 {
        for j in 0..i {
            let sum = list[j] + list[i];
            res += lower_bound(&list, i + 1, list.len(), &sum) - i - 1;
        }
    }
    res.to_string()
}

//(小さい,同じ,大きい)
pub fn bound_count<T: Ord>(vec: &Vec<T>, val: &T) -> (usize, usize, usize) {
    let lb = lower_bound(vec, 0, vec.len(), val);
    let ub = upper_bound(vec, 0, vec.len(), val);
    (lb, ub - lb, vec.len() - ub)
}

//より小さい
pub fn less_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    lower_bound(vec, 0, vec.len(), val)
}

//より大きい
pub fn larger_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    vec.len() - upper_bound(vec, 0, vec.len(), val)
}

//同じ
pub fn eq_count<T: Ord>(vec: &Vec<T>, val: &T) -> usize {
    upper_bound(vec, 0, vec.len(), val) - lower_bound(vec, 0, vec.len(), val)
}

//vecは昇順ソート済み
//以上
pub fn lower_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] < val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] < val {
        last
    } else {
        first
    }
}

//より大きい
pub fn upper_bound<T: Ord>(vec: &Vec<T>, mut first: usize, mut last: usize, val: &T) -> usize {
    let mut mid;
    while last - first > 1 {
        mid = (first + last) / 2;
        if &vec[mid] <= val {
            first = mid;
        } else {
            last = mid;
        }
    }
    if &vec[first] <= val {
        last
    } else {
        first
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
    test1: "4\n3 4 2 1\n" => "1\n",
    test2: "3\n1 1000 1\n" => "0\n",
    test3: "7\n218 786 704 233 645 728 389\n" => "23\n",
}
