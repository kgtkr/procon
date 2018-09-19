extern crate hound;

fn main() {
    let list = read_files();

    print!("{}\t\t", "答え");
    answer(&list);
    println!();

    print!("{}\t", "階差数列の絶対値の和");
    diff(&list);
    println!();

    print!("{}\t", "絶対値の分散");
    variance(&list);
    println!();

    print!("{}\t", "絶対値の合計");
    sample_sum(&list);
    println!();

    print!("{}\t", "絶対値の比較");
    sample(&list);
    println!();
}

type Wav = Vec<i16>;
type WavList = Vec<(Wav, Wav)>;

fn read_files() -> WavList {
    fn read(name: &str) -> Vec<i16> {
        let mut reader = hound::WavReader::open(name).unwrap();
        reader
            .samples::<i16>()
            .map(|x| x.unwrap())
            .collect::<Vec<_>>()
    }

    (1..16)
        .map(|x| {
            (
                read(&format!("input/{:>02}_A.wav", x)),
                read(&format!("input/{:>02}_B.wav", x)),
            )
        })
        .collect::<Vec<_>>()
}

// 答えlist:WavList
fn answer(list: &WavList) {
    print!("{}", "AABAAAABABABBAB");
}

// 階差数列の絶対値の和
fn diff(list: &WavList) {
    for (a, b) in list.clone() {
        let a = run(&a);
        let b = run(&b);
        print!("{}", if a > b { "B" } else { "A" });
    }

    fn run(wav: &Wav) -> i64 {
        wav.iter()
            .enumerate()
            .map(|(i, &x)| match i {
                0 => 0i64,
                _ => ((x as i64) - (wav[i - 1] as i64)).abs(),
            })
            .fold(0i64, |sum, i| sum + i as i64)
    }
}

// 絶対値の分散
fn variance(list: &WavList) {
    for (a, b) in list.clone() {
        let a = run(&a);
        let b = run(&b);
        print!("{}", if a > b { "B" } else { "A" });
    }

    fn run(wav: &Wav) -> i64 {
        let wav = wav.iter().map(|&x| (x as i32).abs()).collect::<Vec<_>>();

        let sum = wav.iter().fold(0i64, |sum, &i| sum + i as i64);
        let count = wav.len() as i64;
        let ave = sum / count;
        wav.iter().map(|&x| (x as i64 - ave).pow(2)).sum::<i64>() / count
    }
}


// サンプルの絶対値の合計
fn sample_sum(list: &WavList) {
    for (a, b) in list.clone() {
        let a = run(&a);
        let b = run(&b);
        print!("{}", if a > b { "A" } else { "B" });
    }

    fn run(wav: &Wav) -> i64 {
        let wav = wav.iter().map(|&x| (x as i32).abs()).collect::<Vec<_>>();

        wav.iter().fold(0i64, |sum, &i| sum + i as i64)
    }
}

// サンプルごとに比較
fn sample(list: &WavList) {
    for (a, b) in list.clone() {
        print!("{}", run(&a, &b));
    }

    fn run(wav_a: &Wav, wav_b: &Wav) -> String {
        let iter_a = wav_a.iter().map(|&x| (x as i32).abs());
        let iter_b = wav_b.iter().map(|&x| (x as i32).abs());

        let (a, b) = iter_a
            .zip(iter_b)
            .map(|(a, b)| if a < b { (0, 1) } else { (1, 0) })
            .fold((0, 0), |(sum_a, sum_b), (a, b)| (sum_a + a, sum_b + b));

        if a > b { "A" } else { "B" }.to_string()
    }
}
