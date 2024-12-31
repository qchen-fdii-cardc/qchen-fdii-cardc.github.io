fun main() {
    val (lb, ub) = 0.0 to 2 * Math.PI

    val func = EvaluationCounter(::fitness)
    val result = jeneticsExample(func, lb, ub)

    println("Minimum found by Jenetics: $result")
    println("Number of evaluations: ${func.evaluations}")

    val minimum = gridSearch(func, lb, ub, 5000)

    println("Minimum found by GridSearch: $minimum")
    println("Number of evaluations: ${func.evaluations}")

    exportFunctionPng(
        minimum,
        "f(x) = cos(0.5 + sin(x)) * cos(x), x in [0, 2π], f'(x), f''(x)",
        "../../jetpack-imgs/jenetics/output.png"
    )

}

