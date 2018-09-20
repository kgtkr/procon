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

    let table1 = (2..50 + 1)
        .filter(|&x| x % 2 == 0)
        .map(|x| len_n(x))
        .enumerate()
        .map(|(i, x)| ((i + 1) * 2, x, true))
        .collect::<Vec<_>>();

    let table2 = (1..15 + 1)
        .map(|x| len_n2(x))
        .enumerate()
        .map(|(i, x)| (i + 1, x, false))
        .collect::<Vec<_>>();

    let mut table = table1.into_iter().chain(table2).collect::<Vec<_>>();
    table.sort_by_key(|&(i, x, flag)| {
        let len = if flag { i } else { i * 4 };
        (x / len as i64, x)
    });
    table.reverse();

    println!("{:?}", table);

    let mut n = n;
    let mut res = Vec::new();
    let mut c = 1;
    for (i, x, flag) in table {
        while n >= x {
            n -= x;
            if flag {
                for _ in 0..i {
                    res.push(c);
                }
                c += 1;
            } else {
                for _ in 0..i {
                    res.push(c);
                }
                for _ in 0..i {
                    res.push(c + 1);
                }
                for _ in 0..i {
                    res.push(c);
                }
                for _ in 0..i {
                    res.push(c + 1);
                }
                c += 2;
            }
        }
    }

    println!("文字の種類:{} 文字列長:{}", c - 1, res.len());

    let len = res.len();
    let res_str = res
        .into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    format!("{}\n{}", len, res_str).to_string()
}

fn len_n2(n: i64) -> i64 {
    2 * len_n(2 * n) + (0..n).map(|i| combi(n, n - i).pow(4)).sum::<i64>()
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
    hoge:"1000000000000"=>"",
}
