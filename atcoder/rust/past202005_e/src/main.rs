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
    input!(input=>(n:usize m:usize q:usize){m;edges:(@,@)}(cs:[usize]){q;qs:[usize]});
    let mut res = Vec::new();

    // 各ノードの色
    let mut cs = cs;
    // 各ノードの隣接
    let mut nodes = Vec::with_capacity(n);
    nodes.resize(n, Vec::new());
    for (a, b) in edges {
        nodes[a].push(b);
        nodes[b].push(a);
    }

    for q in qs {
        match &q[..] {
            &[1, x] => {
                let x = x - 1;
                res.push(cs[x]);
                for &i in &nodes[x] {
                    cs[i] = cs[x];
                }
            }
            &[2, x, y] => {
                let x = x - 1;
                res.push(cs[x]);
                cs[x] = y;
            }
            _ => panic!(),
        }
    }

    res.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
        .to_string()
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
    test1: "3 2 3\n1 2\n2 3\n5 10 15\n1 2\n2 1 20\n1 1\n" => "10\n10\n20\n",
    test2: "30 10 20\n11 13\n30 14\n6 4\n7 23\n30 8\n17 4\n6 23\n24 18\n26 25\n9 3\n18 4 36 46 28 16 34 19 37 23 25 7 24 16 17 41 24 38 6 29 10 33 38 25 47 8 13 8 42 40\n2 1 9\n1 8\n1 9\n2 20 24\n2 26 18\n1 20\n1 26\n2 24 31\n1 4\n2 21 27\n1 25\n1 29\n2 10 14\n2 2 19\n2 15 36\n2 28 6\n2 13 5\n1 12\n1 11\n2 14 43\n" => "18\n19\n37\n29\n8\n24\n18\n25\n46\n10\n18\n42\n23\n4\n17\n8\n24\n7\n25\n16\n",
}
