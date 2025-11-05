/// Return a human-readable name for a moon phase index.
///
/// The `nph` parameter is an index in the range 0..=3 that denotes the
/// canonical quarter phases used by the algorithms in this module:
/// - 0: New Moon
/// - 1: First Quarter
/// - 2: Full Moon
/// - 3: Last Quarter
///
/// Any other value yields "Unknown Phase".
///
/// # Examples
///
/// ```rust
/// assert_eq!(crate::calendar::moon_phase(2), "Full Moon");
/// ```
pub fn moon_phase(nph: i32) -> &'static str {
    match nph {
        0 => "New Moon",
        1 => "First Quarter",
        2 => "Full Moon",
        3 => "Last Quarter",
        _ => "Unknown Phase",
    }
}

/// Compute an approximate Julian day and fractional day offset for a lunar
/// phase.
///
/// This implements a classic lunar phase approximation. Parameters:
/// - `n`: lunation index (count of lunations since a reference epoch)
/// - `nph`: phase index (0 = New, 1 = First Quarter, 2 = Full, 3 = Last Quarter)
///
/// Returns a tuple `(jd, frac)` where `jd` is the integral Julian day for the
/// phase and `frac` is the fractional day offset (in days) into that day.
///
/// The algorithm follows common astronomical approximations and is suitable
/// for general-purpose calendrical calculations (not for high-precision
/// ephemeris work).
pub fn flmoon(n: i64, nph: i32) -> (i64, f64) {
    const RAD: f64 = std::f64::consts::PI / 180.0;
    let n = n as f64;
    let nph = nph as f64;
    let c = n + nph / 4.0;
    let t = c / 1236.85;
    let t2 = t * t;
    let _as = 359.2242 + 29.105356 * c;
    let _am = 306.0253 + 385.816918 * c + 0.010730 * t2;
    let jd = 2415020.0 + 28.0 * n + 7.0 * nph;
    let xtra = 0.75933 + 1.53058868 * c + (1.178e-4 - 1.55e-7 * t) * t2;
    let xtra = xtra
        + if nph == 0.0 || nph == 2.0 {
            (0.1734 - 3.93e-4 * t) * (_as * RAD).sin() - 0.4068 * (_am * RAD).sin()
        } else if nph == 1.0 || nph == 3.0 {
            (0.1721 - 4.0e-4 * t) * (_as * RAD).sin() - 0.6280 * (_am * RAD).sin()
        } else {
            panic!("bad nph {}", nph)
        };

    let i = if xtra > 0.0 {
        xtra.floor() as i64
    } else {
        (xtra - 1.0).ceil() as i64
    };

    let jd = (jd + i as f64) as i64;
    let frac = xtra - i as f64;

    (jd, frac)
}

/// Compute the Julian Day Number for a given Gregorian (or Julian) date.
///
/// Arguments:
/// - `month`: 1-based month (1 = January)
/// - `day`: day of the month
/// - `year`: the year (note: there is no year zero; negative years are BCE)
///
/// The function follows the common algorithm that handles the Gregorian
/// calendar reform (October 1582). Returns the integer Julian day number.
pub fn julday(month: i32, day: i32, year: i32) -> i64 {
    const IGREG: i32 = 15 + 31 * (10 + 12 * 1582);
    let jy = if year == 0 {
        panic!("julday: there is no year zero");
    } else if year < 0 {
        year + 1
    } else {
        year
    };

    let jy = if month > 2 { jy } else { jy - 1 };

    let jm = if month > 2 { month + 1 } else { month + 13 };

    let jd = (365.25 * jy as f64).floor() + (30.6001 * jm as f64).floor() + day as f64 + 1720995.0;
    let jd = jd as i64;

    if day + 31 * (month + 12 * jy) >= IGREG {
        let ja = (0.01 * jy as f64).floor() as i64;
        jd + 2 - ja + (0.25 * ja as f64).floor() as i64
    } else {
        jd
    }
}

/// Convert an integral Julian day number to a calendar date (year, month, day).
///
/// The returned tuple is `(year, month, day)` using the proleptic Gregorian
/// calendar for dates on/after the reform and the Julian calendar before it.
pub fn caldat(jd: i64) -> (i32, i32, i32) {
    const IGREG: i64 = 2299161;
    let ja = if jd >= IGREG {
        let jalpha = (((jd as f64 - 1867216.25) / 36524.25).floor()) as i64;
        jd + 1 + jalpha - (0.25 * jalpha as f64).floor() as i64
    } else {
        jd
    };

    let jb = ja + 1524;
    let jc = ((6680.0 + ((jb as f64 - 2439870.0) - 122.1) / 365.25).floor()) as i64;
    let jd = (365.0 * jc as f64 + (0.25 * jc as f64).floor()) as i64;
    let je = ((jb - jd) as f64 / 30.6001).floor() as i64;

    let day = jb - jd - (30.6001 * je as f64).floor() as i64;
    let month = if je > 13 { je - 13 } else { je - 1 };
    let year = if month > 2 { jc - 4716 } else { jc - 4715 };

    (year as i32, month as i32, day as i32)
}

/// Convert a fractional day (0.0..1.0) into hours, minutes, seconds and the
/// fractional part of a second.
///
/// Returns `(hours, minutes, seconds, fractional_seconds)` where `fractional_seconds`
/// is the fractional remainder of the second (between 0 and 1).
pub fn time_of_day(frac: f64) -> (i32, i32, i32, f64) {
    let total_seconds = frac * 86400.0;
    let seconds = total_seconds.floor() as i32;
    let factor_seconds = total_seconds - (seconds as f64);
    let hours = seconds / 3600;
    let minutes = (seconds % 3600) / 60;
    let seconds = seconds % 60;
    (hours, minutes, seconds, factor_seconds)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_moon_phase() {
        assert_eq!(moon_phase(0), "New Moon");
        assert_eq!(moon_phase(1), "First Quarter");
        assert_eq!(moon_phase(2), "Full Moon");
        assert_eq!(moon_phase(3), "Last Quarter");
        assert_eq!(moon_phase(4), "Unknown Phase");
    }

    #[test]
    fn test_flmoon() {
        let (jd, frac) = flmoon(0, 0);
        assert_eq!(jd, 2415021);
        assert!((frac - 0.08598468).abs() < 1e-5);
    }

    #[test]
    fn test_julday() {
        let jd = julday(1, 1, 2000);
        assert_eq!(jd, 2451545);
    }

    #[test]
    fn test_caldat() {
        let (year, month, day) = caldat(2451545);
        assert_eq!((year, month, day), (2000, 1, 1));
    }

    #[test]
    fn test_time_of_day() {
        let (h, m, s, fs) = time_of_day(0.5);
        assert_eq!((h, m, s), (12, 0, 0));
        assert!((fs - 0.0).abs() < 1e-5);
    }
}
