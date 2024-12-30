import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.ValueMarker
import java.awt.BasicStroke
import java.awt.Color
import kotlin.math.abs
import kotlin.math.cos
import kotlin.math.sin

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

fun fitness(d: Double): Double {
    return cos(0.5 + sin(d)) * cos(d)
}

fun exportFunctionPng(minimum: FunctionPointAndDerivatives) {
    val xData = linspace(0.0, 2.0 * Math.PI, 1000)
    val yData = xData.map { fitness(it) }
    val yData1 = xData.map { derive(::fitness, it) }
    val yData2 = xData.map { derive2(::fitness, it) }

    // using jfreechart to plot the data and save it to a png file

    val dataset = createDataset(xData, yData, yData1, yData2)
    val chart = ChartFactory.createXYLineChart(
        "Fitness, First Derivative, and Second Derivative",
        "x",
        "y",
        dataset,
        PlotOrientation.VERTICAL,
        true,
        true,
        false
    )


    chart.xyPlot.addDomainMarker(ValueMarker(minimum.point, Color.CYAN, BasicStroke(1.0f)))
    chart.xyPlot.addRangeMarker(ValueMarker(minimum.fitness, Color.CYAN, BasicStroke(1.0f)))

    saveChartAsPNG(chart, "output.png")
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