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
    input!(input=>(n:i64));
    /*
    nを2以上の偶数

    繰り返さない場合
    f(n)=nC(n-2*0)+nC(n-2*1)+nC(n-2*2)+...+nC(n-2*(n/2-1))

    (文字列aをn回)(文字列bをn回)(文字列aをn回)(文字列bをn回)
    f'(n)=2f(2n)+α(n)
    α(n)=(nC(n-0))^4+(nC(n-1))^4+...+(nC(n-(n-1)))^4

    aa c=1,n=1
    abab c=2,n=1
    abcabc c=3,n=1
    aabbaabb c=2,n=2
    g(c,n)=β(1,c,n)+β(2,c,n)..β(c,c,n)
    β(i,c,n)=
        f(2n) (i==1)
        cCi((nC1)^2i+(nC2)^2i+...+(nCn)^2i) (else)

    f(2n)=g(1,n)
    g(1,n)=β(1,1,n)=(nC1)^2+(nC2)^2+...+(nCn)^2

    f'(n)=g(2,n)
    */

    let mut table = (1..20)
        .flat_map(|c| {
            let c = c;
            (1..(20 / c))
                .map(|n| (c, n, len_n2(c, n)))
                .collect::<Vec<_>>()
                .into_iter()
        })
        .collect::<Vec<_>>();

    table.sort_by_key(|&(c, n, x)| {
        let len = 2 * n * c;
        (x * 10000 / len as i64)
    });
    table.reverse();

    //println!("{:?}", table);

    let mut now_n = n;
    let mut res = Vec::new();
    let mut now_c = 1;
    for (c, n, x) in table {
        while now_n >= x {
            now_n -= x;
            for _ in 0..2 {
                for z in 0..c {
                    for _ in 0..n {
                        res.push(now_c + z);
                    }
                }
            }
            now_c += c;
        }
    }

    //println!("文字の種類:{} 文字列長:{}", now_c, res.len());

    let len = res.len();
    let res_str = res
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    format!("{}\n{}", len, res_str).to_string()
}

//aaabbbaaabbb
//1ブロックn文字、c種類の文字種から任意文字種選んで
fn len_n2(c: i64, n: i64) -> i64 {
    //1ブロックn文字、c種類の文字からなるやつからi文字種選んで
    fn f(i: i64, c: i64, n: i64) -> i64 {
        if i == 1 {
            c * len_n(2 * n)
        } else {
            //nから1〜n選ぶパターン数の合計
            let count = (1..n + 1).map(|x| combi(n, x)).sum::<i64>();

            //文字種の選び方
            combi(c, i) * (count * 2).pow(i as u32)
        }
    }

    (1..c + 1).map(|x| f(x, c, n)).sum::<i64>()
}

fn len_n(n: i64) -> i64 {
    (0..n / 2).map(|i| combi(n, n - 2 * i)).sum::<i64>()
}

fn combi(n: i64, r: i64) -> i64 {
    if r == 0 {
        1
    } else {
        (n - r + 1) * combi(n, r - 1) / r
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
    test1: "7" => "4\n1 1 1 1",
    test2: "299" => "23\n32 11 11 73 45 8 11 83 83 8 45 32 32 10 100 73 32 83 45 73 32 11 10",
}
