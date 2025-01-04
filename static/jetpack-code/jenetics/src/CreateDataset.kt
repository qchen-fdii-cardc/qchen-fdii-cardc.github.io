import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYSeriesCollection

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


