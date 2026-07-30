use crate::{scatter_plot, Image, Pixel};

fn main() -> std::io::Result<()> {
    // let width = 256;
    // let height = 256;
    // let center_x = width as f64 / 2.0;
    // let center_y = height as f64 / 2.0;
    // let ring_width = 8.0;

    // let file = File::create("output.ppm")?;
    // let mut writer = BufWriter::new(file);

    // writeln!(writer, "P6")?;
    // writeln!(writer, "{} {}", width, height)?;
    // writeln!(writer, "255")?;

    // for y in 0..height {
    //     for x in 0..width {
    //         let dx = x as f64 - center_x;
    //         let dy = y as f64 - center_y;
    //         let radius = (dx * dx + dy * dy).sqrt();
    //         let ring_index = (radius / ring_width) as i32;
    //         let color = if ring_index % 2 == 0 { 255 } else { 0 };
    //         writer.write_all(&[color, color, color])?;
    //     }
    // }

    // println!("Generated output.ppm");

    let width = 256;
    let height = 256;

    let image = Image::from_pixel_fn(width, height, |x, y| {
        let center_x = width as f64 / 2.0;
        let center_y = height as f64 / 2.0;
        let ring_width = 8.0;

        let dx = x as f64 - center_x;
        let dy = y as f64 - center_y;
        let radius = (dx * dx + dy * dy).sqrt();
        let ring_index = (radius / ring_width) as i32;
        let color_value = if ring_index % 2 == 0 { 255 } else { 0 };

        Pixel {
            r: color_value,
            g: color_value,
            b: color_value,
        }
    });

    image.to_file("output.ppm")?;

    // scatter plot example
    let points = vec![(0.0, 0.0), (1.0, 1.0), (2.0, 4.0), (3.0, 9.0), (4.0, 16.0)];
    let xlim = (0.0, 4.0);
    let ylim = (0.0, 16.0);

    let scatter_image = scatter_plot(&points, xlim, ylim, 256, 256);
    scatter_image.to_file("scatter_plot.ppm")
}
