import kotlin.math.abs

fun gridSearch(
    func: EvaluationCounter<Double, Double>,
    lb: Double,
    ub: Double,
    points: Int
): FunctionPointAndDerivatives {
    func.reset()
    val minimum = linspace(lb, ub, points).map {
        FunctionPointAndDerivatives.of(it, func)
    }.filter {
        it.secondDerivative > 0
    }.minBy { abs(it.firstDerivative) }
    return minimum
}