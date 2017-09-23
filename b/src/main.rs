use std::io;

fn main() {
    let line = io::stdin().read_line().unwrap();
    let split: Vec<&str> = line.as_slice().split(' ').collect();
    let n: i64 = from_str(split[0].trim()).unwrap();
    let m: i64 = from_str(split[1].trim()).unwrap();
    let k: i64 = from_str(split[2].trim()).unwrap();


}
