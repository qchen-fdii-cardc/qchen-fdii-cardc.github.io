import org.jfree.chart.ChartUtils
import org.jfree.chart.JFreeChart
import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection
import java.io.File

fun createDataset(
    xData: Array<Double>, yData: List<Double>, yData1: List<Double>, yData2: List<Double>
): XYSeriesCollection {
    val dataset = XYSeriesCollection()

    XYSeries("Fitness").apply {
        xData.forEachIndexed { index, x -> add(x, yData[index]) }
        dataset.addSeries(this)
    }
    XYSeries("First Derivative").apply {
        xData.forEachIndexed { index, x -> add(x, yData1[index]) }
        dataset.addSeries(this)
    }
    XYSeries("Second Derivative").apply {
        xData.forEachIndexed { index, x -> add(x, yData2[index]) }
        dataset.addSeries(this)
    }
    return dataset
}

fun saveChartAsPNG(chart: JFreeChart, filePath: String, width: Int = 1024, height: Int = 768) {
    ChartUtils.saveChartAsPNG(File(filePath), chart, width, height)
}
