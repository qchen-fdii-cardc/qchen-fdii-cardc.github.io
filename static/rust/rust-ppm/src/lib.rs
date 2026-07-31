#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]

pub mod image;
pub use image::{Image, Pixel};

pub mod plot;
pub mod ppm;

pub use plot::{
    line_plot, line_plot_with_width, line_plot_with_width_and_inset, scatter_plot,
    scatter_plot_with_size, scatter_plot_with_size_and_inset,
};

pub mod ppmplot {
    pub use crate::plot::*;
}
