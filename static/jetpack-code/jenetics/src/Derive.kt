
fun derive(f: (Double) -> Double, x: Double, h: Double = 1e-6): Double {
    return (f(x + h) - f(x - h)) / (2 * h)
}


fun derive2(f: (Double) -> Double, x: Double, h: Double = 1e-6): Double {
    return (f(x + h) - 2 * f(x) + f(x - h)) / (h * h)
}


fun linspace(start: Double, stop: Double, num: Int): Array<Double> {
    val step = (stop - start) / (num - 1)
    return Array(num) { i -> start + i * step }
}



data class FunctionPointAndDerivatives(
    val point: Double, val fitness: Double, val firstDerivative: Double, val secondDerivative: Double
) {
    companion object {
        private var _h = 1e-6
        var h: Double
            get() = _h
            set(value) {
                _h = value
            }

        fun of(x: Double, f: (Double) -> Double = { it }): FunctionPointAndDerivatives {
            return FunctionPointAndDerivatives(x, f(x), derive(f, x, h), derive2(f, x, h))
        }
    }
}