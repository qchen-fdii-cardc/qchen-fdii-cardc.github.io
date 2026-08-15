use rust_ppm::{Image, Pixel};

fn blend_u8(background: u8, foreground: u8, alpha: f32) -> u8 {
    (background as f32 * (1.0 - alpha) + foreground as f32 * alpha).round() as u8
}

fn supersample_ring_coverage(
    x: usize,
    y: usize,
    center_x: f32,
    center_y: f32,
    radius: f32,
    thickness: f32,
    samples: usize,
) -> f32 {
    let mut inside = 0usize;
    let step = 1.0 / samples as f32;

    for sub_y in 0..samples {
        for sub_x in 0..samples {
            let px = x as f32 + (sub_x as f32 + 0.5) * step;
            let py = y as f32 + (sub_y as f32 + 0.5) * step;
            let dx = px - center_x;
            let dy = py - center_y;
            let distance = (dx * dx + dy * dy).sqrt();
            let d = (distance - radius).abs();

            if d <= thickness / 2.0 {
                inside += 1;
            }
        }
    }

    inside as f32 / (samples * samples) as f32
}

fn main() -> std::io::Result<()> {
    let image = Image::from_pixel_fn(256, 256, |x, y| Pixel::rgb(x as u8, y as u8, 200));

    image.save("gradient.ppm")?;

    const SIZE: usize = 320;

    let image2 = Image::from_pixel_fn(SIZE, SIZE, |x, y| {
        // same thickness semantics as the AA version: ring width is defined by
        // distance from the ideal radius, not by a squared-distance hack.
        let center_x = SIZE as f32 / 2.0;
        let center_y = SIZE as f32 / 2.0;
        let radius = SIZE as f32 / 2.0 - 10.0;
        let thickness = radius * 0.01;

        let dx = x as f32 - center_x;
        let dy = y as f32 - center_y;
        let distance = (dx * dx + dy * dy).sqrt();
        let d = (distance - radius).abs();

        if d <= thickness / 2.0 {
            Pixel::rgb(255, 0, 0)
        } else {
            Pixel::rgb(255, 255, 255)
        }
    });

    image2.save("circle.ppm")?;

    // Anti-aliased circular outline: a ring with a controllable line thickness.
    let image3 = Image::from_pixel_fn(SIZE, SIZE, |x, y| {
        let center_x = SIZE as f32 / 2.0;
        let center_y = SIZE as f32 / 2.0;
        let radius = SIZE as f32 / 2.0 - 10.0;
        let thickness = radius * 0.01;

        let dx = x as f32 - center_x;
        let dy = y as f32 - center_y;
        let distance = (dx * dx + dy * dy).sqrt();

        // ring thickness is measured as the distance from the ideal circle radius
        let d = (distance - radius).abs();
        let coverage = 1.0 - (d - thickness / 2.0 + 0.5).clamp(0.0, 1.0);
        let background: [u8; 3] = [255_u8, 255_u8, 255_u8];
        let foreground: [u8; 3] = [255_u8, 0_u8, 0_u8];
        let [bg_r, bg_g, bg_b] = background;
        let [fg_r, fg_g, fg_b] = foreground;

        Pixel::rgb(
            blend_u8(bg_r, fg_r, coverage),
            blend_u8(bg_g, fg_g, coverage),
            blend_u8(bg_b, fg_b, coverage),
        )
    });

    image3.save("circle_aa_outline.ppm")?;

    // Higher-quality anti-aliased outline using supersampling.
    // Same geometry as the existing examples for a clean comparison.
    let image4 = Image::from_pixel_fn(SIZE, SIZE, |x, y| {
        let center_x = SIZE as f32 / 2.0;
        let center_y = SIZE as f32 / 2.0;
        let radius = SIZE as f32 / 2.0 - 10.0;
        let thickness = radius * 0.01;

        let coverage = supersample_ring_coverage(x, y, center_x, center_y, radius, thickness, 4);
        let background: [u8; 3] = [255_u8, 255_u8, 255_u8];
        let foreground: [u8; 3] = [255_u8, 0_u8, 0_u8];
        let [bg_r, bg_g, bg_b] = background;
        let [fg_r, fg_g, fg_b] = foreground;

        Pixel::rgb(
            blend_u8(bg_r, fg_r, coverage),
            blend_u8(bg_g, fg_g, coverage),
            blend_u8(bg_b, fg_b, coverage),
        )
    });

    image4.save("circle_aa_super.ppm")?;

    Ok(())
}
