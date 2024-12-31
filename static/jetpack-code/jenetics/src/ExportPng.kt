import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtils
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.ValueMarker
import java.awt.BasicStroke
import java.awt.Color
import java.io.File

fun saveChartAsPNG(chart: JFreeChart, filePath: String, width: Int = 1024, height: Int = 768) {
    ChartUtils.saveChartAsPNG(File(filePath), chart, width, height)
}

fun exportFunctionPng(minimum: FunctionPointAndDerivatives) {
    val xData = linspace(0.0, 2.0 * Math.PI, 1000)
    val yData = xData.map { fitness(it) }
    val yData1 = xData.map { derive(::fitness, it) }
    val yData2 = xData.map { derive2(::fitness, it) }

    // using jfreechart to plot the data and save it to a png file

    val dataset = createDataset(xData, yData, yData1, yData2)
    val chart = ChartFactory.createXYLineChart(
        "f(x) = cos(0.5 + sin(x)) * cos(x), x in [0, 2π], f'(x), f''(x)",
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

    saveChartAsPNG(chart, "../../jetpack-imgs/jenetics/output.png")
}