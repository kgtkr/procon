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
    input!(input=>(x: usize y: usize  z: usize  k: usize)(a_list: [i64])(b_list: [i64])(c_list: [i64]));
    let mut ab_list = Vec::new();
    for a in &a_list {
        for b in &b_list {
            ab_list.push(a + b);
        }
    }
    ab_list.sort();
    ab_list.reverse();

    let ab_list = ab_list.into_iter().take(k).collect::<Vec<_>>();

    let mut abc_list = Vec::new();
    for ab in &ab_list {
        for c in &c_list {
            abc_list.push(ab + c);
        }
    }

    abc_list.sort();
    abc_list.reverse();
    let abc_list = abc_list.into_iter().take(k).collect::<Vec<_>>();

    abc_list
        .iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("\n")
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
    test1: "2 2 2 8\n4 6\n1 5\n3 8\n" => "19\n17\n15\n14\n13\n12\n10\n8\n",
    test2: "3 3 3 5\n1 10 100\n2 20 200\n1 10 100\n" => "400\n310\n310\n301\n301\n",
    test3: "10 10 10 20\n7467038376 5724769290 292794712 2843504496 3381970101 8402252870 249131806 6310293640 6690322794 6082257488\n1873977926 2576529623 1144842195 1379118507 6003234687 4925540914 3902539811 3326692703 484657758 2877436338\n4975681328 8974383988 2882263257 7690203955 514305523 6679823484 4263279310 585966808 3752282379 620585736\n" => "23379871545\n22444657051\n22302177772\n22095691512\n21667941469\n21366963278\n21287912315\n21279176669\n21160477018\n21085311041\n21059876163\n21017997739\n20703329561\n20702387965\n20590247696\n20383761436\n20343962175\n20254073196\n20210218542\n20150096547\n",
}
