use std::io;
use std::num;
use std::vec::Vec;

fn read_stdin() -> Vec<i64> {
    //https://gist.github.com/hebiyan/cb56a7c7eb7c210ce562
    /* 標準入力を開いて */
    let mut scan = std::io::stdin();

    let mut line = String::new();

    /* 読み込んで  */
    let _ = scan.read_line(&mut line);
    /* 空白で分割してからベクタにまとめる */
    let vec: Vec<&str> = line.split_whitespace().collect();

    vec.iter().map(|&x| x.parse().unwrap()).collect::<Vec<_>>()
}

fn main() {
    let input = read_stdin();
    let h = input[0];
    let w = input[1];
    let d = input[2];

    //距離dの座標のペアを求める
    (1..h).map(|y| {
        (1..w).map(|x| {
            //y,xからの距離がdであるのは
            (1..h).map(|y1| {
                (1..w).map(|x1| if ((y - y1).abs() + (x - x1).abs() == d) {
                    Some(((y, x), (y1, x1)))
                } else {
                    None
                })
            })
        })
    })
}