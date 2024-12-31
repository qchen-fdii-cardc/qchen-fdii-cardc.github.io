import kotlin.math.abs
import kotlin.math.cos
import kotlin.math.sin


fun fitness(d: Double): Double {
    return cos(0.5 + sin(d)) * cos(d)
}

fun main() {
    val func = EvaluationCounter(::fitness)
    val result = jeneticsExample(func, 0.0, 2 * Math.PI)

    println("Minimum found by Jenetics: $result")
    println("Number of evaluations: ${func.evaluations}")

    func.reset()
    val minimum = linspace(0.0, 2.0 * Math.PI, 1000).map {
        FunctionPointAndDerivatives.apply { h = 1e-6 }.of(it, func)
    }.filter {
        it.secondDerivative > 0
    }.minBy { abs(it.firstDerivative) }

    println("Minimum found by GridSearch: $minimum")
    println("Number of evaluations: ${func.evaluations}")

    exportFunctionPng(minimum)

}