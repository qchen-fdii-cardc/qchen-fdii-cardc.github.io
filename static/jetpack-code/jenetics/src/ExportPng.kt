import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtils
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.plot.ValueMarker
import java.awt.BasicStroke
import java.awt.Color
import java.io.File

fun saveChartAsPNG(chart: JFreeChart, filePath: String, width: Int = 800, height: Int = 600) {
    ChartUtils.saveChartAsPNG(File(filePath), chart, width, height)
}

fun exportFunctionPng(minimum: FunctionPointAndDerivatives, title: String, fn: String) {
    val xData = linspace(0.0, 2.0 * Math.PI, 1000)
    val yData = xData.map { fitness(it) }
    val yData1 = xData.map { derive(::fitness, it) }
    val yData2 = xData.map { derive2(::fitness, it) }

    // using jfreechart to plot the data and save it to a png file

    val dataset = createDataset(xData, yData, yData1, yData2)
    val chart = ChartFactory.createXYLineChart(
        title,
        "x",
        "y",
        dataset,
        PlotOrientation.VERTICAL,
        true,
        true,
        false
    )

    // set linewidth for all series
    chart.xyPlot.renderer.setSeriesStroke(0, BasicStroke(4.0f))
    chart.xyPlot.renderer.setSeriesStroke(1, BasicStroke(4.0f))
    chart.xyPlot.renderer.setSeriesStroke(2, BasicStroke(4.0f))

    chart.xyPlot.backgroundPaint = Color.WHITE


    chart.xyPlot.addDomainMarker(ValueMarker(minimum.point, Color.CYAN, BasicStroke(1.0f)))
    chart.xyPlot.addRangeMarker(ValueMarker(minimum.fitness, Color.CYAN, BasicStroke(1.0f)))

    saveChartAsPNG(chart, fn)
}