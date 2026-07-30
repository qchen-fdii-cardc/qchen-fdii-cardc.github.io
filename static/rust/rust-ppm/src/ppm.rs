use std::fs::File;
use std::io::{BufRead, BufWriter, Read, Write};

#[derive(Clone, Copy)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

pub struct Image {
    pub width: usize,
    pub height: usize,
    pixels: Vec<Pixel>,
}

impl Image {
    pub fn new(width: usize, height: usize) -> Self {
        let pixels = vec![Pixel { r: 0, g: 0, b: 0 }; width * height];
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn new_black(width: usize, height: usize) -> Self {
        let pixels = vec![Pixel { r: 0, g: 0, b: 0 }; width * height];
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn new_white(width: usize, height: usize) -> Self {
        let pixels = vec![
            Pixel {
                r: 255,
                g: 255,
                b: 255
            };
            width * height
        ];
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn from_color(width: usize, height: usize, color: Pixel) -> Self {
        let pixels = vec![color; width * height];
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn from_pixels(width: usize, height: usize, pixels: Vec<Pixel>) -> Self {
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn from_pixel_fn(width: usize, height: usize, f: impl Fn(usize, usize) -> Pixel) -> Self {
        let mut pixels = Vec::with_capacity(width * height);
        for y in 0..height {
            for x in 0..width {
                pixels.push(f(x, y));
            }
        }
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn from_file(filename: &str) -> std::io::Result<Self> {
        read_ppm(filename)
    }

    pub fn from_image(image: &Image) -> Self {
        Image {
            width: image.width,
            height: image.height,
            pixels: image.pixels.clone(),
        }
    }

    pub fn to_file(&self, filename: &str) -> std::io::Result<()> {
        write_ppm(self, filename)
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> Option<&Pixel> {
        if x < self.width && y < self.height {
            Some(&self.pixels[y * self.width + x])
        } else {
            None
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, pixel: Pixel) {
        if x < self.width && y < self.height {
            self.pixels[y * self.width + x] = pixel;
        }
    }
}

fn write_ppm(image: &Image, filename: &str) -> std::io::Result<()> {
    let file = File::create(filename)?;
    let mut writer = BufWriter::new(file);

    writeln!(writer, "P6")?;
    writeln!(writer, "{} {}", image.width, image.height)?;
    writeln!(writer, "255")?;

    for pixel in &image.pixels {
        writer.write_all(&[pixel.r, pixel.g, pixel.b])?;
    }

    Ok(())
}

fn read_ppm(filename: &str) -> std::io::Result<Image> {
    let file = File::open(filename)?;
    let mut reader = std::io::BufReader::new(file);
    let mut header = String::new();

    reader.read_line(&mut header)?;
    if header.trim() != "P6" {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Not a PPM file",
        ));
    }

    let mut dimensions = String::new();
    reader.read_line(&mut dimensions)?;
    let dims: Vec<&str> = dimensions.trim().split_whitespace().collect();
    if dims.len() != 2 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Invalid dimensions",
        ));
    }
    let width: usize = dims[0]
        .parse()
        .map_err(|_| std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid width"))?;
    let height: usize = dims[1]
        .parse()
        .map_err(|_| std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid height"))?;

    let mut max_color_value = String::new();
    reader.read_line(&mut max_color_value)?;
    if max_color_value.trim() != "255" {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Unsupported max color value",
        ));
    }

    let mut pixels = Vec::with_capacity(width * height);
    for _ in 0..(width * height) {
        let mut rgb = [0u8; 3];
        reader.read_exact(&mut rgb)?;
        pixels.push(Pixel {
            r: rgb[0],
            g: rgb[1],
            b: rgb[2],
        });
    }

    Ok(Image {
        width,
        height,
        pixels,
    })
}
