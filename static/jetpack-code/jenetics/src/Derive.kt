fun derive(f: (Double) -> Double, x: Double, h: Double = 1e-6): Double {
    return (8 * f(x + h) + f(x - 2 * h) - (f(x + 2 * h) + 8 * f(x - h))) / (12.0 * h)
}


fun derive2(f: (Double) -> Double, x: Double, h: Double = 1e-6): Double {
    return (16 * (f(x + h) + f(x - h)) - (30 * f(x) + f(x + 2 * h) + f(x - 2 * h))) / (12.0 * h * h)
}
