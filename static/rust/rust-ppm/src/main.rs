use std::fs::File;
use std::io::{BufWriter, Write};

fn main() -> std::io::Result<()> {
    let width = 256;
    let height = 256;
    let center_x = width as f64 / 2.0;
    let center_y = height as f64 / 2.0;
    let ring_width = 8.0;

    let file = File::create("output.ppm")?;
    let mut writer = BufWriter::new(file);

    writeln!(writer, "P6")?;
    writeln!(writer, "{} {}", width, height)?;
    writeln!(writer, "255")?;

    for y in 0..height {
        for x in 0..width {
            let dx = x as f64 - center_x;
            let dy = y as f64 - center_y;
            let radius = (dx * dx + dy * dy).sqrt();
            let ring_index = (radius / ring_width) as i32;
            let color = if ring_index % 2 == 0 { 255 } else { 0 };

            writer.write_all(&[color, color, color])?;
        }
    }

    println!("Generated output.ppm");
    Ok(())
}
