//! Binary PPM (`P6`) encoding and decoding.

use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::path::Path;

pub use crate::{Image, Pixel};

/// Writes an image as a binary `P6` PPM file.
pub fn write(image: &Image, path: impl AsRef<Path>) -> io::Result<()> {
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);

    write_to(image, &mut writer)
}

fn write_to(image: &Image, mut writer: impl Write) -> io::Result<()> {
    writeln!(writer, "P6")?;
    writeln!(writer, "{} {}", image.width, image.height)?;
    writeln!(writer, "255")?;

    for pixel in image.pixels() {
        writer.write_all(&[pixel.r, pixel.g, pixel.b])?;
    }

    Ok(())
}

/// Reads a binary `P6` PPM file with a maximum channel value of `255`.
pub fn read(path: impl AsRef<Path>) -> io::Result<Image> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);

    read_from(&mut reader)
}

fn read_from(mut reader: impl BufRead) -> io::Result<Image> {
    let mut header = String::new();

    reader.read_line(&mut header)?;
    if header.trim() != "P6" {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "Not a PPM file"));
    }

    let mut dimensions = String::new();
    reader.read_line(&mut dimensions)?;
    let dims: Vec<&str> = dimensions.split_whitespace().collect();
    if dims.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Invalid dimensions",
        ));
    }
    let width: usize = dims[0]
        .parse()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid width"))?;
    let height: usize = dims[1]
        .parse()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid height"))?;

    let mut max_color_value = String::new();
    reader.read_line(&mut max_color_value)?;
    if max_color_value.trim() != "255" {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Unsupported max color value",
        ));
    }

    let pixel_count = width
        .checked_mul(height)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Image dimensions overflow"))?;
    let mut pixels = Vec::with_capacity(pixel_count);
    for _ in 0..pixel_count {
        let mut rgb = [0u8; 3];
        reader.read_exact(&mut rgb)?;
        pixels.push(Pixel {
            r: rgb[0],
            g: rgb[1],
            b: rgb[2],
        });
    }

    Ok(Image::from_pixels(width, height, pixels))
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, Cursor};

    use super::*;

    #[test]
    fn image_round_trips_through_binary_ppm() {
        let image = Image::from_pixels(2, 1, vec![Pixel::rgb(1, 2, 3), Pixel::rgb(250, 251, 252)]);
        let mut encoded = Vec::new();
        write_to(&image, &mut encoded).unwrap();

        let decoded = read_from(BufReader::new(Cursor::new(encoded))).unwrap();
        assert_eq!(decoded, image);
    }

    #[test]
    fn oversized_dimensions_return_invalid_data() {
        let input = format!("P6\n{} 2\n255\n", usize::MAX);
        let error = read_from(BufReader::new(Cursor::new(input))).unwrap_err();

        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn truncated_pixel_data_returns_unexpected_eof() {
        let input = b"P6\n1 1\n255\n\x01\x02";
        let error = read_from(BufReader::new(Cursor::new(input))).unwrap_err();

        assert_eq!(error.kind(), io::ErrorKind::UnexpectedEof);
    }
}
