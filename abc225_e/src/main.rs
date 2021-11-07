extern crate core;

use num_rational::Ratio;
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
    input!(input=>(n:usize){n;list: (i64,i64)});
    // 四角形をy=1の1次元に変換
    let mut list = list
        .into_iter()
        .map(|(x, y)| {
            let x_1 = x - 1;
            let y_1 = y;
            let x_2 = x;
            let y_2 = y - 1;
            (Ratio::new_raw(x_1, y_1), {
                if y_2 != 0 {
                    Ratio::new_raw(x_2, y_2)
                } else {
                    Ratio::new_raw(std::i64::MAX, 1)
                }
            })
        })
        .collect::<Vec<_>>();

    list.sort_by_key(|&(_, x)| x);

    let mut ans = 0;
    let mut prev = Ratio::new_raw(0, 1);
    for (a, b) in list {
        if a < prev {
            continue;
        }
        ans += 1;
        prev = b;
    }

    ans.to_string()
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
    test1: "3\n1 1\n2 1\n1 2\n" => "2\n",
    test2: "10\n414598724 87552841\n252911401 309688555\n623249116 421714323\n605059493 227199170\n410455266 373748111\n861647548 916369023\n527772558 682124751\n356101507 249887028\n292258775 110762985\n850583108 796044319\n" => "10\n",
}
