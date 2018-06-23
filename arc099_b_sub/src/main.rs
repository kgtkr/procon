fn main() {
    println!("[");
    for x in (1..100000).map(sum) {
        println!("[{},{}],", x.0, x.1);
    }
    println!("]");
}

fn sum(x: i64) -> (i64, f64) {
    (
        x,
        x as f64
            / x.to_string()
                .chars()
                .map(|x| x.to_string().parse::<f64>().unwrap())
                .sum::<f64>(),
    )
}
