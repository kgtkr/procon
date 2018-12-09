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
    input!(input=>(n:usize m:usize)(list:[i64]){m;m_list:(@,@)});
    let mut dp = Vec::with_capacity(n);
    dp.resize(n, -1);
    f(&list, &m_list_cov(n, m_list), &mut dp, 0).to_string()
}

fn m_list_cov(n: usize, mut m_list: Vec<(usize, usize)>) -> Vec<usize> {
    let mut res = Vec::with_capacity(n);
    res.resize(n, 0);

    m_list.sort_by_key(|x| (x.1, x.0));
    m_list.reverse();
    // now〜n-1まで書き換え済み
    let mut now = n;
    for (l, r) in m_list {
        for i in l..std::cmp::min(now, r + 1) {
            res[i] = r;
        }
        now = l;
    }

    res
}

// i番目以降の最大値
fn f(list: &Vec<i64>, m_list: &Vec<usize>, dp: &mut Vec<i64>, i: usize) -> i64 {
    if i >= list.len() {
        0
    } else if dp[i] != -1 {
        dp[i]
    } else {
        // どこから置けるか
        // ここの高速化したい
        let now = std::cmp::max(i + 1, m_list[i] + 1);
        let res = std::cmp::max(
            f(list, m_list, dp, i + 1),
            list[i] + f(list, m_list, dp, now),
        );
        dp[i] = res;
        res
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
    test1: "4 1
1 2 3 8
2 4" => "9",
    test2: "5 2
2 3 9 5 6
1 3
2 4" => "15",
    test3: "20 10
870851814 594414687 615919461 65033245 460143082 617460823 881870957 126041265 623075703 34130727 27054628 853567651 483228744 491145755 220689940 148007930 229257101 790404982 612186806 281076231
15 19
20 20
12 13
1 4
19 19
9 13
3 6
9 12
16 16
18 19" => "4912419478",
}
