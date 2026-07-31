use crate::{Image, Pixel};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct Bounds {
    pub left: usize,
    pub top: usize,
    pub right: usize,
    pub bottom: usize,
}

impl Bounds {
    pub fn from_insets(
        width: usize,
        height: usize,
        inset_x: usize,
        inset_y: usize,
    ) -> Option<Self> {
        let (left, right) = inset_bounds(width, inset_x)?;
        let (top, bottom) = inset_bounds(height, inset_y)?;
        Some(Self {
            left,
            top,
            right,
            bottom,
        })
    }

    pub fn width(self) -> usize {
        self.right - self.left + 1
    }

    pub fn height(self) -> usize {
        self.bottom - self.top + 1
    }
}

pub(super) fn draw_box(image: &mut Image, bounds: Bounds, color: Pixel) {
    for x in bounds.left..=bounds.right {
        image.set_pixel(x, bounds.top, color);
        image.set_pixel(x, bounds.bottom, color);
    }
    for y in bounds.top..=bounds.bottom {
        image.set_pixel(bounds.left, y, color);
        image.set_pixel(bounds.right, y, color);
    }
}

pub(super) fn draw_marker(
    image: &mut Image,
    center: (usize, usize),
    size: usize,
    bounds: Bounds,
    color: Pixel,
) {
    if size == 0 {
        return;
    }

    let lower_radius = (size - 1) / 2;
    let upper_radius = size / 2;
    let start_x = center.0.saturating_sub(lower_radius).max(bounds.left);
    let end_x = (center.0 + upper_radius).min(bounds.right);
    let start_y = center.1.saturating_sub(lower_radius).max(bounds.top);
    let end_y = (center.1 + upper_radius).min(bounds.bottom);

    for y in start_y..=end_y {
        for x in start_x..=end_x {
            image.set_pixel(x, y, color);
        }
    }
}

pub(super) fn draw_thick_line(
    image: &mut Image,
    start: (usize, usize),
    end: (usize, usize),
    width: usize,
    bounds: Bounds,
    color: Pixel,
) {
    walk_line(start, end, |x, y| {
        if x >= bounds.left && x <= bounds.right && y >= bounds.top && y <= bounds.bottom {
            draw_marker(image, (x, y), width, bounds, color);
        }
    });
}

pub(super) fn draw_axis_arrows(
    image: &mut Image,
    y_axis_x: usize,
    x_axis_y: usize,
    bounds: Bounds,
    color: Pixel,
) {
    let arrow_length = 6;
    let half_width = 4;
    let right_base = bounds.right.saturating_sub(arrow_length).max(bounds.left);

    draw_line(
        image,
        (bounds.right, x_axis_y),
        (
            right_base,
            x_axis_y.saturating_sub(half_width).max(bounds.top),
        ),
        color,
    );
    draw_line(
        image,
        (bounds.right, x_axis_y),
        (right_base, (x_axis_y + half_width).min(bounds.bottom)),
        color,
    );

    let top_base = (bounds.top + arrow_length).min(bounds.bottom);
    draw_line(
        image,
        (y_axis_x, bounds.top),
        (
            y_axis_x.saturating_sub(half_width).max(bounds.left),
            top_base,
        ),
        color,
    );
    draw_line(
        image,
        (y_axis_x, bounds.top),
        ((y_axis_x + half_width).min(bounds.right), top_base),
        color,
    );
}

pub(super) fn draw_line(
    image: &mut Image,
    start: (usize, usize),
    end: (usize, usize),
    color: Pixel,
) {
    walk_line(start, end, |x, y| image.set_pixel(x, y, color));
}

fn walk_line(start: (usize, usize), end: (usize, usize), mut visit: impl FnMut(usize, usize)) {
    let (mut x, mut y) = (start.0 as i32, start.1 as i32);
    let (end_x, end_y) = (end.0 as i32, end.1 as i32);
    let dx = (end_x - x).abs();
    let dy = -(end_y - y).abs();
    let step_x = if x < end_x { 1 } else { -1 };
    let step_y = if y < end_y { 1 } else { -1 };
    let mut error = dx + dy;

    loop {
        visit(x as usize, y as usize);
        if x == end_x && y == end_y {
            break;
        }
        let doubled_error = 2 * error;
        if doubled_error >= dy {
            error += dy;
            x += step_x;
        }
        if doubled_error <= dx {
            error += dx;
            y += step_y;
        }
    }
}

fn inset_bounds(size: usize, inset: usize) -> Option<(usize, usize)> {
    if size == 0 {
        return None;
    }
    let inset = inset.min((size - 1) / 2);
    Some((inset, size - 1 - inset))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axes_have_right_and_up_arrowheads() {
        let mut image = Image::new_white(21, 21);
        let bounds = Bounds::from_insets(21, 21, 0, 0).unwrap();
        draw_axis_arrows(&mut image, 10, 10, bounds, Pixel::BLACK);

        assert_eq!(image.get_pixel(14, 6), Some(&Pixel::BLACK));
        assert_eq!(image.get_pixel(14, 14), Some(&Pixel::BLACK));
        assert_eq!(image.get_pixel(6, 6), Some(&Pixel::BLACK));
    }
}
