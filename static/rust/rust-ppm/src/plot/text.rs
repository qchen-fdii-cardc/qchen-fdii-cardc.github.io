use font8x8::{BASIC_FONTS, UnicodeFonts};

use crate::{Image, Pixel};

pub(super) const GLYPH_HEIGHT: usize = 8;
const GLYPH_WIDTH: usize = 8;
const GLYPH_SPACING: usize = 1;

pub(super) fn measure(text: &str, scale: usize) -> (usize, usize) {
    let count = text.chars().count();
    if count == 0 || scale == 0 {
        return (0, 0);
    }

    let width = (count * (GLYPH_WIDTH + GLYPH_SPACING) - GLYPH_SPACING) * scale;
    (width, GLYPH_HEIGHT * scale)
}

pub(super) fn draw(
    image: &mut Image,
    origin: (usize, usize),
    text: &str,
    scale: usize,
    color: Pixel,
) {
    visit_pixels(text, scale, |x, y| {
        image.set_pixel(origin.0 + x, origin.1 + y, color);
    });
}

pub(super) fn draw_rotated_counterclockwise(
    image: &mut Image,
    origin: (usize, usize),
    text: &str,
    scale: usize,
    color: Pixel,
) {
    let (width, _) = measure(text, scale);
    visit_pixels(text, scale, |x, y| {
        image.set_pixel(origin.0 + y, origin.1 + width - 1 - x, color);
    });
}

fn visit_pixels(text: &str, scale: usize, mut visit: impl FnMut(usize, usize)) {
    if scale == 0 {
        return;
    }

    let advance = (GLYPH_WIDTH + GLYPH_SPACING) * scale;
    for (character_index, character) in text.chars().enumerate() {
        let Some(glyph) = BASIC_FONTS.get(character) else {
            continue;
        };

        for (row, bits) in glyph.into_iter().enumerate() {
            for column in 0..GLYPH_WIDTH {
                if bits & (1 << column) == 0 {
                    continue;
                }
                let pixel_x = character_index * advance + column * scale;
                let pixel_y = row * scale;
                for offset_y in 0..scale {
                    for offset_x in 0..scale {
                        visit(pixel_x + offset_x, pixel_y + offset_y);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn text_is_rasterized_into_pixels() {
        let mut image = Image::new_white(32, 16);
        draw(&mut image, (1, 1), "x1", 1, Pixel::BLACK);

        assert!(image.pixels().contains(&Pixel::BLACK));
        assert_eq!(measure("x1", 1), (17, 8));
    }
}
