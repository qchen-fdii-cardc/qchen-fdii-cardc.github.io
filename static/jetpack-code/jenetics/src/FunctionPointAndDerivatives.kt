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