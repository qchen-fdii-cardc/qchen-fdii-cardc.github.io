# Circle Rendering in PPM

This project generates a few simple images in the PPM format using Rust and the `rust_ppm` crate. The main focus is to render a circular outline and demonstrate the difference between a hard-edge circle and a classic anti-aliased circle.

## Setting up

To set up the project, ensure you have Rust installed. Then, clone the repository and build the project:

```bash
git clone https://github.com/qchen-fdii-cardc/circle-rust-ppm.git
cd circle
cargo build --release
```

run the program to generate the images:

```bash
cargo run
```

using the provided script, you can convert the generated PPM files to PNG format:

```bash
./convert_ppm_to_png.sh
```



## Rendering concept

The core idea is to evaluate every pixel in the image and decide whether it belongs to the circle ring.

For a pixel at `(x, y)`, we compute:

- center position: `(center_x, center_y)`
- distance from the center:

```rust
let dx = x as f32 - center_x;
let dy = y as f32 - center_y;
let distance = (dx * dx + dy * dy).sqrt();
```

Then we compare this distance against the target radius:

```rust
let d = (distance - radius).abs();
```

If the value is inside the ring thickness, the pixel is painted red; otherwise it stays white.

This is the exact logic used in the hard-edge version:

```rust
if d <= thickness / 2.0 {
    Pixel::rgb(255, 0, 0)
} else {
    Pixel::rgb(255, 255, 255)
}
```

This produces a clean circular outline with a controlled line thickness.

## Hard-edge ring

The first circle version uses a fixed-width ring based on the distance from the ideal radius:

```rust
let center_x = SIZE as f32 / 2.0;
let center_y = SIZE as f32 / 2.0;
let radius = SIZE as f32 / 2.0 - 10.0;
let thickness = radius * 0.05;

let dx = x as f32 - center_x;
let dy = y as f32 - center_y;
let distance = (dx * dx + dy * dy).sqrt();
let d = (distance - radius).abs();

if d <= thickness / 2.0 {
    Pixel::rgb(255, 0, 0)
} else {
    Pixel::rgb(255, 255, 255)
}
```

Mathematically this is simply:

$$
\text{ring}(p) = \left|\|p - c\| - r\right| \le \frac{t}{2}
$$

where:

- $p$ is the pixel position
- $c$ is the circle center
- $r$ is the radius
- $t$ is the line thickness

This is mathematically simple and visually sharp, but the edges can look jagged on screen.

## Classic anti-aliasing algorithm

The anti-aliased version keeps the same geometric idea, but instead of a binary decision, it computes a smooth coverage value around the boundary.

```rust
let d = (distance - radius).abs();
let coverage = 1.0 - (d - thickness / 2.0 + 0.5).clamp(0.0, 1.0);
```

The coverage is defined as a smooth falloff near the ring boundary:

$$
\text{coverage}(p) = \mathrm{clamp}\left(1 - \left(\left|\|p-c\|-r\right| - \frac{t}{2} + 0.5\right), 0, 1\right)
$$

This value represents how much of the pixel is inside the circle. It is then used to blend the red foreground with the white background:

```rust
let background = [255_u8, 255_u8, 255_u8];
let foreground = [255_u8, 0_u8, 0_u8];
let [bg_r, bg_g, bg_b] = background;
let [fg_r, fg_g, fg_b] = foreground;

Pixel::rgb(
    blend_u8(bg_r, fg_r, coverage),
    blend_u8(bg_g, fg_g, coverage),
    blend_u8(bg_b, fg_b, coverage),
)
```

The blending function is:

```rust
fn blend_u8(background: u8, foreground: u8, alpha: f32) -> u8 {
    (background as f32 * (1.0 - alpha) + foreground as f32 * alpha).round() as u8
}
```

which is the standard linear interpolation formula:

$$
C = (1 - \alpha)B + \alpha F
$$

This is the classic anti-aliasing technique: the color transitions gradually over a 1-pixel boundary region, which visually reduces stair-step artifacts.

## Supersampling anti-aliasing algorithm

The supersampling version keeps the same ring geometry but evaluates multiple sub-samples inside each pixel before averaging them.

```rust
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
```

The mathematical idea is to estimate the pixel coverage by averaging many local samples:

$$
\text{coverage}_{SS} = \frac{1}{N^2} \sum_{i=1}^{N^2} \mathbf{1}\left(\left|\|p_i-c\|-r\right| \le \frac{t}{2}\right)
$$

where:

- $N$ is the supersampling factor (here, 4)
- $p_i$ are the sub-pixel sample positions inside the pixel
- $\mathbf{1}(\cdot)$ is 1 when the sample falls inside the ring and 0 otherwise

This produces a better approximation to the continuous boundary than a single-sample coverage check, so the circle edge looks smoother.

The final pixel color uses the same blending formula:

$$
C = (1 - \text{coverage}_{SS})B + \text{coverage}_{SS}F
$$

This is the more advanced AA strategy used in the `circle_aa_super` example.

## Example output

When the program runs, it generates images such as:

- `gradient.ppm`
- `circle.ppm`
- `circle_aa_outline.ppm`
- `circle_aa_super.ppm`

These are created with `Image::from_pixel_fn` and saved to disk using `save(...)`.

## Run it

```bash
cargo run
```

Then convert PPM files to PNG if needed:

```bash
./convert_ppm_to_png.sh
```

or manually:

```bash
convert circle.ppm circle.png
convert circle_aa_outline.ppm circle_aa_outline.png
convert circle_aa_super.ppm circle_aa_super.png
```

## Summary

The project demonstrates three rendering styles:

1. Hard-edge ring drawing, based on a simple distance threshold.
2. Classic anti-aliased ring drawing, based on a coverage value and alpha blending.
3. Supersampled anti-aliased ring drawing, based on averaging multiple local samples per pixel.

This makes it easy to compare the visual quality of different AA strategies on the same geometric circle.

## Run it

```bash
cargo run
```

Then convert PPM files to PNG if needed:

```bash
./convert_ppm_to_png.sh
```

or manually:

```bash
convert circle.ppm circle.png
convert circle_aa_outline.ppm circle_aa_outline.png
```

## Summary

The project demonstrates two common rendering styles:

1. Hard-edge ring drawing, based on a simple distance threshold.
2. Anti-aliased ring drawing, based on a coverage value and alpha blending.

The anti-aliased version is the classic solution for smoother edges in raster graphics.
