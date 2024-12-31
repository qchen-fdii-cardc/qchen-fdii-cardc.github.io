class EvaluationCounter<in T, out R>(val f: (T) -> R) : (T) -> R {
    var evaluations = 0
    override fun invoke(p1: T): R {
        evaluations++
        return f(p1)
    }

    fun reset() {
        evaluations = 0
    }
}
