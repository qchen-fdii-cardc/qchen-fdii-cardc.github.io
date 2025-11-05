mod calendar;

fn main() {
    for n in 0..=100 {
        for nph in 0..4 {
            let (year, frac) = calendar::flmoon(n, nph);
            let moon_phase = calendar::moon_phase(nph);
            let (y, m, d) = calendar::caldat(year);
            let (h, min, s, ss) = calendar::time_of_day(frac);
            print!("{:>4}th {:>15} {:>12} {:>20}", n, moon_phase, year, frac);
            println!(
                " => {:>4}{:>02}{:>02}+{:>02}:{:>02}:{:>02}:{:>12}",
                y, m, d, h, min, s, ss
            );
        }
    }

    for year in 2025..=2025 {
        for month in 11..=12 {
            for day in 1..=31 {
                let jd = calendar::julday(month, day, year);
                print!("{:>4}-{:>02}-{:>02} => JD {}", year, month, day, jd);
                let (y, m, d) = calendar::caldat(jd);
                println!("  JD {} => {:>4}-{:>02}-{:>02}", jd, y, m, d);
            }
        }
    }

    let n = 1280 + 23 * 12;

    let (y, f) = calendar::flmoon(n, 2);
    println!("{}th Full Moon: JD {}, frac {}", n, y, f);
    let (y, m, d) = calendar::caldat(y);
    let (h, min, s, ss) = calendar::time_of_day(f);
    println!(
        "=> {:>4}-{:>02}-{:>02}+{:>02}:{:>02}:{:>02}:{:>12}",
        y, m, d, h, min, s, ss
    );

    let y = calendar::julday(11, 5, 2025);
    println!("JD of 2025-11-05: {}", y);
}
