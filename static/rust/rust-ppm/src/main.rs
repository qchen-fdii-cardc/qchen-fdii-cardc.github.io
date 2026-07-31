use std::io;

use rust_ppm::plot::{Axes, Canvas};
use rust_ppm::{Image, Pixel};

const IMAGE_SIZE: usize = 512;
const PLOT_INSET_X: usize = 60;
const PLOT_INSET_Y: usize = 36;

fn main() -> io::Result<()> {
    rings_image().save("output.ppm")?;

    let points = [
        (-1.0, -1.0),
        (0.0, 0.0),
        (2.0, 4.0),
        (3.0, 9.0),
        (4.0, 16.0),
    ];
    let axes = Axes::from_limits((-2.0, 4.0), (-2.0, 16.0))
        .with_labels("x", "x squared")
        .with_title("Quadratic samples");

    let mut scatter = plot_canvas(axes.clone());
    scatter.render();
    scatter.scatter(&points, 4);
    scatter.into_image().save("scatter_plot.ppm")?;

    let mut line = plot_canvas(axes);
    line.render();
    line.plot(&points, 4);
    line.into_image().save("line_plot.ppm")
}

fn rings_image() -> Image {
    let center = IMAGE_SIZE as f64 / 2.0;

    Image::from_pixel_fn(IMAGE_SIZE, IMAGE_SIZE, |x, y| {
        let dx = x as f64 - center;
        let dy = y as f64 - center;
        let radius = (dx * dx + dy * dy).sqrt();
        let value = if ((radius / 8.0) as u32).is_multiple_of(2) {
            255
        } else {
            0
        };
        Pixel::rgb(value, value, value)
    })
}

fn plot_canvas(axes: Axes) -> Canvas {
    Canvas::with_inset(IMAGE_SIZE, IMAGE_SIZE, axes, PLOT_INSET_X, PLOT_INSET_Y)
}
