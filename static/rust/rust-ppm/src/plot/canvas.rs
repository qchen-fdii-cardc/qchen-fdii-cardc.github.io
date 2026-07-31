use crate::{Image, Pixel};

use super::Axes;
use super::raster::{Bounds, draw_axis_arrows, draw_box, draw_line, draw_marker, draw_thick_line};
use super::text::{self, GLYPH_HEIGHT};

const TICK_LENGTH: usize = 4;
const TEXT_GAP: usize = 2;

/// An image-backed plotting surface.
pub struct Canvas {
    pub axes: Axes,
    pub image: Image,
    inset_x: usize,
    inset_y: usize,
}

impl Canvas {
    pub fn new(width: usize, height: usize, axes: Axes) -> Self {
        Self::with_inset(width, height, axes, 0, 0)
    }

    pub fn with_inset(
        width: usize,
        height: usize,
        axes: Axes,
        inset_x: usize,
        inset_y: usize,
    ) -> Self {
        Self {
            axes,
            image: Image::new_white(width, height),
            inset_x,
            inset_y,
        }
    }

    pub fn width(&self) -> usize {
        self.image.width
    }

    pub fn height(&self) -> usize {
        self.image.height
    }

    pub fn insets(&self) -> (usize, usize) {
        (self.inset_x, self.inset_y)
    }

    pub fn set_inset(&mut self, inset_x: usize, inset_y: usize) {
        self.inset_x = inset_x;
        self.inset_y = inset_y;
    }

    pub fn render(&mut self) {
        let Some(bounds) = self.bounds() else {
            return;
        };
        self.draw_axes(bounds);
    }

    pub fn scatter(&mut self, points: &[(f64, f64)], size: usize) {
        let Some(bounds) = self.bounds() else {
            return;
        };

        for &(x, y) in points {
            if !self.contains(x, y) {
                continue;
            }

            if let Some(pixel) = self.map_point((x, y), bounds) {
                draw_marker(&mut self.image, pixel, size, bounds, Pixel::rgb(255, 0, 0));
            }
        }
    }

    pub fn plot(&mut self, points: &[(f64, f64)], line_width: usize) {
        let Some(bounds) = self.bounds() else {
            return;
        };

        let mut previous = None;
        for &(x, y) in points {
            if !self.contains(x, y) {
                previous = None;
                continue;
            }

            if let Some(pixel) = self.map_point((x, y), bounds) {
                if let Some(start) = previous {
                    draw_thick_line(
                        &mut self.image,
                        start,
                        pixel,
                        line_width,
                        bounds,
                        Pixel::rgb(0, 0, 255),
                    );
                } else {
                    draw_marker(
                        &mut self.image,
                        pixel,
                        line_width,
                        bounds,
                        Pixel::rgb(0, 0, 255),
                    );
                }
                previous = Some(pixel);
            }
        }
    }

    pub fn into_image(self) -> Image {
        self.image
    }

    fn bounds(&self) -> Option<Bounds> {
        Bounds::from_insets(self.width(), self.height(), self.inset_x, self.inset_y)
    }

    fn contains(&self, x: f64, y: f64) -> bool {
        x >= self.axes.xlim.0
            && x <= self.axes.xlim.1
            && y >= self.axes.ylim.0
            && y <= self.axes.ylim.1
    }

    fn map_point(&self, point: (f64, f64), bounds: Bounds) -> Option<(usize, usize)> {
        Some((
            map_x(point.0, self.axes.xlim, bounds)?,
            map_y(point.1, self.axes.ylim, bounds)?,
        ))
    }

    fn draw_axes(&mut self, bounds: Bounds) {
        draw_box(&mut self.image, bounds, Pixel::BLACK);

        let y_axis_x = if contains_zero(self.axes.xlim) {
            map_x(0.0, self.axes.xlim, bounds)
        } else {
            Some(bounds.left + (bounds.right - bounds.left) / 2)
        };
        let x_axis_y = if contains_zero(self.axes.ylim) {
            map_y(0.0, self.axes.ylim, bounds)
        } else {
            Some(bounds.top + (bounds.bottom - bounds.top) / 2)
        };

        if let Some(x) = y_axis_x {
            for y in bounds.top..=bounds.bottom {
                self.image.set_pixel(x, y, Pixel::BLACK);
            }
        }
        if let Some(y) = x_axis_y {
            for x in bounds.left..=bounds.right {
                self.image.set_pixel(x, y, Pixel::BLACK);
            }
        }

        self.draw_ticks(bounds);

        if let (Some(y_axis_x), Some(x_axis_y)) = (y_axis_x, x_axis_y) {
            draw_axis_arrows(&mut self.image, y_axis_x, x_axis_y, bounds, Pixel::BLACK);
        }

        self.draw_labels(bounds);
    }

    fn draw_ticks(&mut self, bounds: Bounds) {
        let bottom_margin = self.height() - 1 - bounds.bottom;

        for &tick in &self.axes.xticks {
            let Some(x) = map_x(tick, self.axes.xlim, bounds) else {
                continue;
            };
            let tick_end = (bounds.bottom + TICK_LENGTH).min(self.height() - 1);
            draw_line(
                &mut self.image,
                (x, bounds.bottom),
                (x, tick_end),
                Pixel::BLACK,
            );

            if bottom_margin >= TICK_LENGTH + TEXT_GAP + GLYPH_HEIGHT {
                let label = format_tick(tick);
                let (label_width, _) = text::measure(&label, 1);
                let label_x = centered_start(x, label_width, self.width());
                text::draw(
                    &mut self.image,
                    (label_x, bounds.bottom + TICK_LENGTH + TEXT_GAP),
                    &label,
                    1,
                    Pixel::BLACK,
                );
            }
        }

        for &tick in &self.axes.yticks {
            let Some(y) = map_y(tick, self.axes.ylim, bounds) else {
                continue;
            };
            let tick_start = bounds.left.saturating_sub(TICK_LENGTH);
            draw_line(
                &mut self.image,
                (tick_start, y),
                (bounds.left, y),
                Pixel::BLACK,
            );

            let label = format_tick(tick);
            let (label_width, label_height) = text::measure(&label, 1);
            if self.height() >= label_height && bounds.left >= TICK_LENGTH + TEXT_GAP + label_width
            {
                let label_x = tick_start - TEXT_GAP - label_width;
                let label_y = y
                    .saturating_sub(label_height / 2)
                    .min(self.height() - label_height);
                text::draw(&mut self.image, (label_x, label_y), &label, 1, Pixel::BLACK);
            }
        }
    }

    fn draw_labels(&mut self, bounds: Bounds) {
        let bottom_margin = self.height() - 1 - bounds.bottom;

        if !self.axes.title.is_empty() && bounds.top >= GLYPH_HEIGHT + TEXT_GAP {
            let (width, _) = text::measure(&self.axes.title, 1);
            let x = centered_start((bounds.left + bounds.right) / 2, width, self.width());
            text::draw(
                &mut self.image,
                (x, bounds.top - GLYPH_HEIGHT - TEXT_GAP),
                &self.axes.title,
                1,
                Pixel::BLACK,
            );
        }

        let xlabel_y = bounds.bottom + TICK_LENGTH + TEXT_GAP + GLYPH_HEIGHT + TEXT_GAP;
        if !self.axes.xlabel.is_empty() && bottom_margin >= xlabel_y - bounds.bottom + GLYPH_HEIGHT
        {
            let (width, _) = text::measure(&self.axes.xlabel, 1);
            let x = centered_start((bounds.left + bounds.right) / 2, width, self.width());
            text::draw(
                &mut self.image,
                (x, xlabel_y),
                &self.axes.xlabel,
                1,
                Pixel::BLACK,
            );
        }

        let max_tick_width = self
            .axes
            .yticks
            .iter()
            .map(|&tick| text::measure(&format_tick(tick), 1).0)
            .max()
            .unwrap_or(0);
        let required_left = TICK_LENGTH + TEXT_GAP + max_tick_width + TEXT_GAP + GLYPH_HEIGHT;
        if !self.axes.ylabel.is_empty() && bounds.left >= required_left {
            let (rotated_height, _) = text::measure(&self.axes.ylabel, 1);
            let x = bounds.left - required_left;
            let y = centered_start(
                (bounds.top + bounds.bottom) / 2,
                rotated_height,
                self.height(),
            );
            text::draw_rotated_counterclockwise(
                &mut self.image,
                (x, y),
                &self.axes.ylabel,
                1,
                Pixel::BLACK,
            );
        }
    }
}

fn map_x(value: f64, limits: (f64, f64), bounds: Bounds) -> Option<usize> {
    map_value(value, limits, bounds.width()).map(|pixel| bounds.left + pixel)
}

fn map_y(value: f64, limits: (f64, f64), bounds: Bounds) -> Option<usize> {
    map_value(value, limits, bounds.height()).map(|pixel| bounds.bottom - pixel)
}

fn map_value(value: f64, limits: (f64, f64), size: usize) -> Option<usize> {
    if limits.1 <= limits.0 || size == 0 || value < limits.0 || value > limits.1 {
        return None;
    }
    let scaled = (value - limits.0) / (limits.1 - limits.0);
    Some((scaled * (size - 1) as f64).round() as usize)
}

fn contains_zero(limits: (f64, f64)) -> bool {
    limits.0 <= 0.0 && 0.0 <= limits.1
}

fn centered_start(center: usize, length: usize, extent: usize) -> usize {
    center
        .saturating_sub(length / 2)
        .min(extent.saturating_sub(length))
}

fn format_tick(value: f64) -> String {
    if value == 0.0 {
        return "0".to_owned();
    }

    let magnitude = value.abs();
    if !(0.001..10_000.0).contains(&magnitude) {
        return format!("{value:.1e}");
    }

    format!("{value:.2}")
        .trim_end_matches('0')
        .trim_end_matches('.')
        .to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coordinates_start_at_bottom_left() {
        let bounds = Bounds::from_insets(7, 7, 0, 0).unwrap();
        assert_eq!(map_x(-2.0, (-2.0, 4.0), bounds), Some(0));
        assert_eq!(map_x(4.0, (-2.0, 4.0), bounds), Some(6));
        assert_eq!(map_y(-2.0, (-2.0, 16.0), bounds), Some(6));
        assert_eq!(map_y(16.0, (-2.0, 16.0), bounds), Some(0));
    }

    #[test]
    fn canvas_respects_horizontal_and_vertical_insets() {
        let mut canvas =
            Canvas::with_inset(11, 11, Axes::from_limits((0.0, 1.0), (0.0, 1.0)), 2, 3);
        canvas.render();

        assert_eq!(canvas.image.get_pixel(0, 0), Some(&Pixel::WHITE));
        assert_eq!(canvas.image.get_pixel(2, 3), Some(&Pixel::BLACK));
        assert_eq!(canvas.image.get_pixel(8, 7), Some(&Pixel::BLACK));
    }

    #[test]
    fn axes_render_origin() {
        let mut canvas = Canvas::new(7, 7, Axes::from_limits((0.0, 2.0), (0.0, 2.0)));
        canvas.render();

        assert_eq!(canvas.image.get_pixel(0, 3), Some(&Pixel::BLACK));
        assert_eq!(canvas.image.get_pixel(3, 6), Some(&Pixel::BLACK));
    }

    #[test]
    fn labels_and_tick_values_render_inside_margins() {
        let axes = Axes::from_limits((0.0, 1.0), (0.0, 1.0))
            .with_labels("time", "value")
            .with_title("demo");
        let mut canvas = Canvas::with_inset(128, 128, axes, 40, 32);
        canvas.render();

        let width = canvas.width();
        let margin_has_text = canvas
            .image
            .pixels()
            .iter()
            .enumerate()
            .any(|(index, pixel)| {
                let x = index % width;
                let y = index / width;
                *pixel == Pixel::BLACK && (x < 36 || !(28..=100).contains(&y))
            });
        assert!(margin_has_text);
    }

    #[test]
    fn tiny_canvas_does_not_panic_while_laying_out_ticks() {
        let mut canvas =
            Canvas::with_inset(64, 1, Axes::from_limits((0.0, 1.0), (0.0, 1.0)), 20, 0);

        canvas.render();
        assert_eq!(canvas.height(), 1);
    }
}
