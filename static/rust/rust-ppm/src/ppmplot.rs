use crate::{Image, Pixel};

pub struct Axes {
    pub xlim: (f64, f64),
    pub ylim: (f64, f64),
    pub xticks: Vec<f64>,
    pub yticks: Vec<f64>,
    pub xlabel: String,
    pub ylabel: String,
    pub title: String,
}

pub struct Canvas {
    pub width: usize,
    pub height: usize,
    pub axes: Axes,
    pub image: Image,
}

fn map_to_pixel(value: f64, min: f64, max: f64, size: usize) -> Option<usize> {
    if max <= min {
        return None;
    }

    let scaled = (value - min) / (max - min);
    let clamped = scaled.clamp(0.0, 1.0);
    Some((clamped * (size as f64 - 1.0)).round() as usize)
}

// plot x-y axes with xlim, ylim, on given Image of given width and height
fn plot_axes(image: &mut Image, xlim: (f64, f64), ylim: (f64, f64)) {
    let width = image.width;
    let height = image.height;

    // Draw y-axis at x = 0 when it lies inside the x-range.
    if xlim.0 <= 0.0 && 0.0 <= xlim.1 {
        if let Some(x_axis_pos) = map_to_pixel(0.0, xlim.0, xlim.1, width) {
            for y in 0..height {
                image.set_pixel(x_axis_pos, y, Pixel { r: 0, g: 0, b: 0 });
            }
        }
    }

    // Draw x-axis at y = 0 when it lies inside the y-range.
    if ylim.0 <= 0.0 && 0.0 <= ylim.1 {
        if let Some(y_axis_pos) = map_to_pixel(0.0, ylim.0, ylim.1, height) {
            for x in 0..width {
                image.set_pixel(x, y_axis_pos, Pixel { r: 0, g: 0, b: 0 });
            }
        }
    }
}

// plot x - y scatter plot of given curve (x, y) points on an Image of given width and height
pub fn scatter_plot(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
) -> Image {
    let mut image = Image::new_white(width, height);

    // Plot axes
    plot_axes(&mut image, xlim, ylim);

    // Plot points
    for &(x, y) in points {
        if x >= xlim.0 && x <= xlim.1 && y >= ylim.0 && y <= ylim.1 {
            let pixel_x = map_to_pixel(x, xlim.0, xlim.1, width);
            let pixel_y = map_to_pixel(ylim.1 - y, ylim.0, ylim.1, height);

            if let (Some(px), Some(py)) = (pixel_x, pixel_y) {
                if px < width && py < height {
                    image.set_pixel(px, py, Pixel { r: 255, g: 0, b: 0 });
                }
            }
        }
    }

    image
}
