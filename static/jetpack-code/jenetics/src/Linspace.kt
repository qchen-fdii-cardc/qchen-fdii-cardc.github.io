fun linspace(start: Double, stop: Double, num: Int): Array<Double> {
    val step = (stop - start) / (num - 1)
    return Array(num) { i -> start + i * step }
}