/// Axis limits, ticks, labels, and title for a plot.
#[derive(Clone, Debug, PartialEq)]
pub struct Axes {
    pub xlim: (f64, f64),
    pub ylim: (f64, f64),
    pub xticks: Vec<f64>,
    pub yticks: Vec<f64>,
    pub xlabel: String,
    pub ylabel: String,
    pub title: String,
}

impl Axes {
    pub fn from_limits(xlim: (f64, f64), ylim: (f64, f64)) -> Self {
        Self {
            xlim,
            ylim,
            xticks: evenly_spaced_ticks(xlim, 4),
            yticks: evenly_spaced_ticks(ylim, 4),
            xlabel: "x".to_owned(),
            ylabel: "y".to_owned(),
            title: "plot".to_owned(),
        }
    }

    pub fn with_labels(mut self, xlabel: impl Into<String>, ylabel: impl Into<String>) -> Self {
        self.xlabel = xlabel.into();
        self.ylabel = ylabel.into();
        self
    }

    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.title = title.into();
        self
    }
}

fn evenly_spaced_ticks(limits: (f64, f64), intervals: usize) -> Vec<f64> {
    if intervals == 0 {
        return vec![limits.0];
    }

    (0..=intervals)
        .map(|index| limits.0 + (limits.1 - limits.0) * index as f64 / intervals as f64)
        .collect()
}
