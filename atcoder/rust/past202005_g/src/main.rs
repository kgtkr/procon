extern crate core;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
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
    input!(input=>(n:usize x:i64 y:i64){n;list:(i64,i64)});
    let mut xs = list.iter().map(|(x, _)| *x).collect::<Vec<_>>();
    xs.push(0);
    xs.push(x);

    let mut ys = list.iter().map(|(_, y)| *y).collect::<Vec<_>>();
    ys.push(0);
    ys.push(y);

    let right = *xs.iter().max().unwrap() + 1;
    let left = *xs.iter().min().unwrap() - 1;

    let top = *ys.iter().max().unwrap() + 1;
    let bottom = *ys.iter().min().unwrap() - 1;

    let blocks = list.into_iter().collect::<HashSet<_>>();

    let is_empty =
        |(x, y)| left <= x && x <= right && bottom <= y && y <= top && !blocks.contains(&(x, y));

    // 既に調べた座標 -> 最短コスト
    let mut dist = HashMap::new();
    dist.insert((0, 0), 0i64);

    let mut queue = VecDeque::new();
    queue.push_back((0, 0));

    while let Some((cur_x, cur_y)) = queue.pop_front() {
        let cur_cost = *dist.get(&(cur_x, cur_y)).unwrap();

        for next in vec![
            (cur_x + 1, cur_y + 1),
            (cur_x, cur_y + 1),
            (cur_x - 1, cur_y + 1),
            (cur_x + 1, cur_y),
            (cur_x - 1, cur_y),
            (cur_x, cur_y - 1),
        ]
        .into_iter()
        .filter(|&p| is_empty(p) && !dist.contains_key(&p))
        .collect::<Vec<_>>()
        {
            queue.push_back(next);
            let cost = cur_cost + 1;
            dist.insert(next, cost);

            if next == (x, y) {
                return cost.to_string();
            }
        }
    }

    "-1".to_string()
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
    test1: "1 2 2\n1 1\n" => "3\n",
    test2: "1 2 2\n2 1\n" => "2\n",
    test3: "5 -2 3\n1 1\n-1 1\n0 1\n-2 1\n-3 1\n" => "6\n",
}
