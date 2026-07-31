//! RGB pixel and image storage.

use std::io;
use std::path::Path;

/// An 8-bit red, green, blue pixel.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Pixel {
    pub const BLACK: Self = Self::rgb(0, 0, 0);
    pub const WHITE: Self = Self::rgb(255, 255, 255);

    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

/// A row-major RGB image.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Image {
    pub width: usize,
    pub height: usize,
    pixels: Vec<Pixel>,
}

impl Image {
    pub fn new(width: usize, height: usize) -> Self {
        Self::from_color(width, height, Pixel::BLACK)
    }

    pub fn new_black(width: usize, height: usize) -> Self {
        Self::from_color(width, height, Pixel::BLACK)
    }

    pub fn new_white(width: usize, height: usize) -> Self {
        Self::from_color(width, height, Pixel::WHITE)
    }

    pub fn from_color(width: usize, height: usize, color: Pixel) -> Self {
        let pixels = vec![color; pixel_count(width, height)];
        Self {
            width,
            height,
            pixels,
        }
    }

    pub fn from_pixels(width: usize, height: usize, pixels: Vec<Pixel>) -> Self {
        assert_eq!(
            pixels.len(),
            pixel_count(width, height),
            "pixel count must match image dimensions"
        );
        Self {
            width,
            height,
            pixels,
        }
    }

    pub fn from_pixel_fn(
        width: usize,
        height: usize,
        mut pixel_fn: impl FnMut(usize, usize) -> Pixel,
    ) -> Self {
        let mut pixels = Vec::with_capacity(pixel_count(width, height));
        for y in 0..height {
            for x in 0..width {
                pixels.push(pixel_fn(x, y));
            }
        }
        Self::from_pixels(width, height, pixels)
    }

    pub fn open(path: impl AsRef<Path>) -> io::Result<Self> {
        crate::ppm::read(path)
    }

    pub fn save(&self, path: impl AsRef<Path>) -> io::Result<()> {
        crate::ppm::write(self, path)
    }

    pub fn from_file(filename: &str) -> io::Result<Self> {
        Self::open(filename)
    }

    pub fn to_file(&self, filename: &str) -> io::Result<()> {
        self.save(filename)
    }

    pub fn from_image(image: &Self) -> Self {
        image.clone()
    }

    pub fn pixels(&self) -> &[Pixel] {
        &self.pixels
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> Option<&Pixel> {
        self.pixel_index(x, y).map(|index| &self.pixels[index])
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, pixel: Pixel) {
        if let Some(index) = self.pixel_index(x, y) {
            self.pixels[index] = pixel;
        }
    }

    fn pixel_index(&self, x: usize, y: usize) -> Option<usize> {
        (x < self.width && y < self.height).then_some(y * self.width + x)
    }
}

fn pixel_count(width: usize, height: usize) -> usize {
    width
        .checked_mul(height)
        .expect("image dimensions overflow")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pixels_are_row_major_and_bounds_checked() {
        let mut image = Image::new_white(2, 2);
        image.set_pixel(1, 0, Pixel::rgb(1, 2, 3));
        image.set_pixel(2, 0, Pixel::BLACK);

        assert_eq!(image.get_pixel(1, 0), Some(&Pixel::rgb(1, 2, 3)));
        assert_eq!(image.get_pixel(0, 1), Some(&Pixel::WHITE));
        assert_eq!(image.get_pixel(2, 0), None);
        assert_eq!(image.pixels().len(), 4);
    }

    #[test]
    #[should_panic(expected = "pixel count must match image dimensions")]
    fn from_pixels_rejects_wrong_pixel_count() {
        Image::from_pixels(2, 2, vec![Pixel::BLACK]);
    }
}
