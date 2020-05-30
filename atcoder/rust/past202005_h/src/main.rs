extern crate core;

use std::collections::HashSet;
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

struct Input {
    l: i64,
    xs: HashSet<i64>,
    t1: i64,
    t2: i64,
    t3: i64,
}

fn solve(input: String) -> String {
    input!(input=>(n:usize l:i64)(xs:[i64])(t1:i64 t2:i64 t3:i64));
    // 距離0.5を1とする
    let l = l * 2;
    let xs = xs.into_iter().map(|x| x * 2).collect::<HashSet<_>>();
    let t1 = t1 / 2;
    let t2 = t2 / 2;
    let t3 = t3 / 2;

    let input = Input { l, xs, t1, t2, t3 };

    let mut dp = Vec::with_capacity(14);
    dp.resize(14, {
        let mut v = Vec::with_capacity(l as usize + 1);
        v.resize(l as usize + 1, None);
        v
    });

    // 各位置にたどり着く最短時間を求めるDP？
    // 1: 2 -- 0 1
    // 2: 1-2-1 -- 2 ; 3 4 ; 5
    // 3: 1-6-1 -- 6 ; 7 8 9 10 11 12 ; 13
    // この合計パターン
    f(&input, &mut dp, 0, 0)
        .min(f(&input, &mut dp, 0, 2))
        .min(f(&input, &mut dp, 0, 6))
        .to_string()
}

// point, stateの時ゴールまでの最短残り時間
// stateは次する行動
fn f(input: &Input, dp: &mut Vec<Vec<Option<i64>>>, point: i64, state: i64) -> i64 {
    if let Some(res) = dp[state as usize][point as usize] {
        return res;
    }

    // 一回の関数でpointからpoint+1まで進むのでそれに掛かる時間を求める
    // pointを「通り過ぎる」と考える
    let res = if point == input.l {
        0
    } else {
        let is_jump = match state {
            3 | 4 | 7 | 8 | 9 | 10 | 11 | 12 => true,
            0 | 1 | 2 | 5 | 6 | 13 => false,
            _ => panic!(),
        };
        let time = if is_jump { input.t2 } else { input.t1 };

        // 0→1 | 1→2 2→3 | 3→4
        let all_jump = match state {
            4 | 8 | 9 | 10 | 11 | 12 => true,
            0 | 1 | 2 | 3 | 5 | 6 | 7 | 13 => false,
            _ => panic!(),
        };

        let add_time = if !all_jump && input.xs.contains(&point) {
            input.t3 * 2
        } else {
            0
        };

        time + add_time
            + match state {
                1 | 5 | 13 => f(input, dp, point + 1, 0)
                    .min(f(input, dp, point + 1, 2))
                    .min(f(input, dp, point + 1, 6)),
                _ => f(input, dp, point + 1, state + 1),
            }
    };

    dp[state as usize][point as usize] = Some(res);

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
    test1: "2 5\n1 4\n2 2 20\n" => "10\n",
    test2: "4 5\n1 2 3 4\n2 20 100\n" => "164\n",
    test3: "10 19\n1 3 4 5 7 8 10 13 15 17\n2 1000 10\n" => "138\n",
}
