//! Axes, canvas rendering, and line/scatter convenience functions.

mod axes;
mod canvas;
mod raster;
mod text;

pub use axes::Axes;
pub use canvas::Canvas;

use crate::Image;

pub fn scatter_plot(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
) -> Image {
    scatter_plot_with_size(points, xlim, ylim, width, height, 1)
}

pub fn scatter_plot_with_size(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
    size: usize,
) -> Image {
    scatter_plot_with_size_and_inset(
        points,
        xlim,
        ylim,
        width,
        height,
        size,
        default_inset(width),
        default_inset(height),
    )
}

#[allow(clippy::too_many_arguments)]
pub fn scatter_plot_with_size_and_inset(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
    size: usize,
    inset_x: usize,
    inset_y: usize,
) -> Image {
    let mut canvas = Canvas::with_inset(
        width,
        height,
        Axes::from_limits(xlim, ylim),
        inset_x,
        inset_y,
    );
    canvas.render();
    canvas.scatter(points, size);
    canvas.into_image()
}

pub fn line_plot(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
) -> Image {
    line_plot_with_width(points, xlim, ylim, width, height, 1)
}

pub fn line_plot_with_width(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
    line_width: usize,
) -> Image {
    line_plot_with_width_and_inset(
        points,
        xlim,
        ylim,
        width,
        height,
        line_width,
        default_inset(width),
        default_inset(height),
    )
}

#[allow(clippy::too_many_arguments)]
pub fn line_plot_with_width_and_inset(
    points: &[(f64, f64)],
    xlim: (f64, f64),
    ylim: (f64, f64),
    width: usize,
    height: usize,
    line_width: usize,
    inset_x: usize,
    inset_y: usize,
) -> Image {
    let mut canvas = Canvas::with_inset(
        width,
        height,
        Axes::from_limits(xlim, ylim),
        inset_x,
        inset_y,
    );
    canvas.render();
    canvas.plot(points, line_width);
    canvas.into_image()
}

fn default_inset(size: usize) -> usize {
    size / 16
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Pixel;

    #[test]
    fn convenience_plots_respect_insets() {
        let scatter = scatter_plot_with_size_and_inset(
            &[(0.0, 0.0)],
            (0.0, 1.0),
            (0.0, 1.0),
            11,
            11,
            1,
            2,
            3,
        );
        assert_eq!(scatter.get_pixel(2, 7), Some(&Pixel::rgb(255, 0, 0)));
        assert_eq!(scatter.get_pixel(0, 10), Some(&Pixel::WHITE));

        let line = line_plot_with_width_and_inset(
            &[(0.0, 0.0), (1.0, 1.0)],
            (0.0, 1.0),
            (0.0, 1.0),
            11,
            11,
            1,
            2,
            3,
        );
        assert_eq!(line.get_pixel(5, 5), Some(&Pixel::rgb(0, 0, 255)));
        assert_eq!(line.get_pixel(0, 10), Some(&Pixel::WHITE));
    }

    #[test]
    fn line_width_and_marker_size_are_configurable() {
        let line = line_plot_with_width(&[(0.0, 0.5), (1.0, 0.5)], (0.0, 1.0), (0.0, 1.0), 9, 9, 3);
        for y in 3..=5 {
            assert_eq!(line.get_pixel(4, y), Some(&Pixel::rgb(0, 0, 255)));
        }

        let scatter = scatter_plot_with_size(&[(0.5, 0.5)], (0.0, 1.0), (0.0, 1.0), 5, 5, 3);
        assert_eq!(scatter.get_pixel(2, 2), Some(&Pixel::rgb(255, 0, 0)));
    }

    #[test]
    fn axes_fall_back_to_center_when_zero_is_outside_limits() {
        let image = scatter_plot(&[], (1.0, 2.0), (1.0, 2.0), 7, 7);
        assert_eq!(image.get_pixel(3, 3), Some(&Pixel::BLACK));
    }
}
