extern crate hound;

fn main() {
    print!("{}\t\t", "答え");
    answer();
    println!();

    print!("{}\t", "階差数列の絶対値の和");
    diff();
    println!();

    print!("{}\t", "絶対値の分散");
    variance();
    println!();

    print!("{}\t", "絶対値の合計");
    sample_sum();
    println!();

    print!("{}\t", "絶対値の比較");
    sample();
    println!();
}

// 答え
fn answer() {
    print!("{}", "AABAAAABABABBAB");
}

// 階差数列の絶対値の和
fn diff() {
    for x in 1..16 {
        let a = run(&format!("input/{:>02}_A.wav", x));
        let b = run(&format!("input/{:>02}_B.wav", x));
        print!("{}", if a > b { "B" } else { "A" });
    }

    fn run(name: &str) -> i64 {
        let mut reader = hound::WavReader::open(name).unwrap();
        let vec = reader
            .samples::<i32>()
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        vec.iter()
            .enumerate()
            .map(|(i, &x)| match i {
                0 => 0i64,
                _ => ((x as i64) - (vec[i - 1] as i64)).abs(),
            })
            .fold(0i64, |sum, i| sum + i as i64)
    }
}

// 絶対値の分散
fn variance() {
    for x in 1..16 {
        let a = run(&format!("input/{:>02}_A.wav", x));
        let b = run(&format!("input/{:>02}_B.wav", x));
        print!("{}", if a > b { "B" } else { "A" });
    }

    fn run(name: &str) -> i64 {
        let mut reader = hound::WavReader::open(name).unwrap();
        let vec = reader
            .samples::<i16>()
            .map(|x| (x.unwrap() as i32).abs())
            .collect::<Vec<_>>();

        let sum = vec.iter().fold(0i64, |sum, &i| sum + i as i64);
        let count = vec.len() as i64;
        let ave = sum / count;
        vec.iter().map(|&x| (x as i64 - ave).pow(2)).sum::<i64>() / count
    }
}


// サンプルの絶対値の合計
fn sample_sum() {
    for x in 1..16 {
        let a = run(&format!("input/{:>02}_A.wav", x));
        let b = run(&format!("input/{:>02}_B.wav", x));
        print!("{}", if a > b { "A" } else { "B" });
    }

    fn run(name: &str) -> i64 {
        let mut reader = hound::WavReader::open(name).unwrap();
        let vec = reader
            .samples::<i16>()
            .map(|x| (x.unwrap() as i32).abs())
            .collect::<Vec<_>>();

        vec.iter().fold(0i64, |sum, &i| sum + i as i64)
    }
}

// サンプルごとに比較
fn sample() {
    for x in 1..16 {
        print!(
            "{}",
            run(
                &format!("input/{:>02}_A.wav", x),
                &format!("input/{:>02}_B.wav", x)
            )
        );
    }

    fn run(name_a: &str, name_b: &str) -> String {
        let mut reader_a = hound::WavReader::open(name_a).unwrap();
        let iter_a = reader_a.samples::<i16>().map(|x| (x.unwrap() as i32).abs());

        let mut reader_b = hound::WavReader::open(name_b).unwrap();
        let iter_b = reader_b.samples::<i16>().map(|x| (x.unwrap() as i32).abs());

        let (a, b) = iter_a
            .zip(iter_b)
            .map(|(a, b)| if a < b { (0, 1) } else { (1, 0) })
            .fold((0, 0), |(sum_a, sum_b), (a, b)| (sum_a + a, sum_b + b));

        if a > b { "A" } else { "B" }.to_string()
    }
}
