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
    input!(input=>(n:usize){n;mat:[i64]});
    // 列の最初の要素との差、全て等しい場合のみSome
    let col_diff = {
        // 各行の列の差
        let col_diffs = (0..n)
            .map(|i| (0..n).map(|j| mat[i][j] - mat[i][0]).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        if (0..n - 1).all(|i| col_diffs[i] == col_diffs[i + 1]) {
            Some(col_diffs[0].clone())
        } else {
            None
        }
    };

    // 行の最初の要素との差、全て等しい場合のみSome
    let row_diff = {
        // 各の行の差
        let row_diffs = (0..n)
            .map(|i| (0..n).map(|j| mat[j][i] - mat[0][i]).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        if (0..n - 1).all(|i| row_diffs[i] == row_diffs[i + 1]) {
            Some(row_diffs[0].clone())
        } else {
            None
        }
    };

    let result = if let (Some(col_diff), Some(row_diff)) = (col_diff, row_diff) {
        let min_col_diff = col_diff.iter().copied().min().unwrap();
        // 最小値との差
        let col_diff = col_diff
            .into_iter()
            .map(|x| x - min_col_diff)
            .collect::<Vec<_>>();

        let min_row_diff = col_diff.iter().copied().min().unwrap();
        // 最小値との差
        let row_diff = row_diff
            .into_iter()
            .map(|x| x - min_row_diff)
            .collect::<Vec<_>>();

        let addn = mat[0][0] - (col_diff[0] + row_diff[0]);

        if addn >= 0 {
            Some((
                col_diff,
                row_diff.into_iter().map(|x| x + addn).collect::<Vec<_>>(),
            ))
        } else {
            None
        }
    } else {
        None
    };

    // 列の差が全て一致するか
    result
        .map(|(col_diff, row_diff)| {
            format!(
                "Yes\n{}\n{}",
                row_diff
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                col_diff
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        })
        .unwrap_or("No".to_string())
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
    test1: "3\n4 3 5\n2 1 3\n3 2 4\n" => "Yes\n2 0 1\n2 1 3\n",
    test2: "3\n4 3 5\n2 2 3\n3 2 4\n" => "No\n",
}
