# rust-ppm

`rust-ppm` is a small RGB image and plotting library that writes binary PPM (`P6`) files.

## Image API

```rust,no_run
use rust_ppm::{Image, Pixel};

let image = Image::from_pixel_fn(128, 128, |x, y| {
    Pixel::rgb(x as u8, y as u8, 128)
});
image.save("gradient.ppm")?;
# Ok::<(), std::io::Error>(())
```

## Plot API

Use `Canvas` when you need labels, custom insets, or multiple drawing operations:

```rust,no_run
use rust_ppm::plot::{Axes, Canvas};

let axes = Axes::from_limits((-1.0, 4.0), (-2.0, 16.0))
    .with_labels("x", "x squared")
    .with_title("Quadratic samples");
let points = [(-1.0, 1.0), (0.0, 0.0), (2.0, 4.0), (4.0, 16.0)];

let mut canvas = Canvas::with_inset(512, 512, axes, 60, 36);
canvas.render();
canvas.plot(&points, 3);
canvas.scatter(&points, 5);
canvas.into_image().save("plot.ppm")?;
# Ok::<(), std::io::Error>(())
```

The convenience functions `line_plot` and `scatter_plot` remain available for simple plots.

## Notes

- Coordinates use a bottom-left data origin; image pixels are stored from the top-left.
- Text uses a compact 8x8 bitmap font. Labels render only when the configured inset has enough room.
- PPM reading supports binary `P6` images with a maximum channel value of `255`.

## Development

```text
cargo fmt --check
cargo test --all-targets
cargo clippy --all-targets -- -D warnings
cargo doc --no-deps
```