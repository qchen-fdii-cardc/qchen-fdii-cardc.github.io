package org.cardc.fdii.qc.Instruments

import android.app.AlertDialog
import android.content.Context
import android.content.Intent
import android.hardware.Sensor
import android.hardware.SensorEvent
import android.hardware.SensorEventListener
import android.hardware.SensorManager
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.view.MotionEvent
import android.widget.EditText
import android.widget.Toast
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.annotation.RequiresApi
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableFloatStateOf
import androidx.compose.runtime.mutableLongStateOf
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import com.github.mikephil.charting.charts.LineChart
import com.github.mikephil.charting.components.XAxis
import com.github.mikephil.charting.components.YAxis
import com.github.mikephil.charting.data.Entry
import com.github.mikephil.charting.data.LineData
import com.github.mikephil.charting.data.LineDataSet
import com.github.mikephil.charting.listener.ChartTouchListener
import com.github.mikephil.charting.listener.OnChartGestureListener
import com.github.mikephil.charting.utils.ColorTemplate
import org.cardc.fdii.qc.Instruments.ui.theme.FirstApplicationTheme
import java.io.File
import java.io.FileWriter

@Composable
fun SensorChart(
    yawData: List<Entry>,
    pitchData: List<Entry>,
    rollData: List<Entry>,
    modifier: Modifier = Modifier
) {
    val context = LocalContext.current
    val chart = remember { LineChart(context) }

    val yawDataSet = LineDataSet(yawData, "Yaw").apply {
        lineWidth = 2f
        color = ColorTemplate.COLORFUL_COLORS[0]
        axisDependency = YAxis.AxisDependency.LEFT
    }

    val pitchDataSet = LineDataSet(pitchData, "Pitch").apply {
        lineWidth = 2f

        color = ColorTemplate.COLORFUL_COLORS[1]
        axisDependency = YAxis.AxisDependency.LEFT
    }

    val rollDataSet = LineDataSet(rollData, "Roll").apply {
        lineWidth = 2f

        color = ColorTemplate.COLORFUL_COLORS[2]
        axisDependency = YAxis.AxisDependency.LEFT
    }

    val lineData = LineData(yawDataSet, pitchDataSet, rollDataSet)
    chart.data = lineData

    chart.xAxis.position = XAxis.XAxisPosition.BOTTOM
    chart.axisRight.isEnabled = false
    chart.description.isEnabled = false


    // Set gesture listener
    chart.onChartGestureListener = object : OnChartGestureListener {
        override fun onChartGestureStart(
            me: MotionEvent?, lastPerformedGesture: ChartTouchListener.ChartGesture?
        ) {
        }

        override fun onChartGestureEnd(
            me: MotionEvent?, lastPerformedGesture: ChartTouchListener.ChartGesture?
        ) {
        }

        override fun onChartLongPressed(me: MotionEvent?) {}

        @RequiresApi(Build.VERSION_CODES.O)
        override fun onChartDoubleTapped(me: MotionEvent?) {
            showFileNameDialog(context, yawData, pitchData, rollData)
        }

        override fun onChartSingleTapped(me: MotionEvent?) {}
        override fun onChartFling(
            me1: MotionEvent?, me2: MotionEvent?, velocityX: Float, velocityY: Float
        ) {
        }

        override fun onChartScale(me: MotionEvent?, scaleX: Float, scaleY: Float) {}
        override fun onChartTranslate(me: MotionEvent?, dX: Float, dY: Float) {}
    }

    chart.invalidate()
    // Enable auto-scaling
    chart.isAutoScaleMinMaxEnabled = true

    AndroidView({ chart }, modifier = modifier.padding(16.dp).border(1.dp, Color.Gray))
}


class MainActivity : ComponentActivity(), SensorEventListener {
    private lateinit var sensorManager: SensorManager
    private var rotationVectorSensor: Sensor? = null

    private var _yaw by mutableFloatStateOf(0f)
    private var _pitch by mutableFloatStateOf(0f)
    private var _roll by mutableFloatStateOf(0f)

    // add a variable to store the high resolution time
    private val _time0 = System.nanoTime()
    private var _time by mutableLongStateOf(0L)

    override fun onResume() {
        super.onResume()
        rotationVectorSensor?.also { sensor ->
            sensorManager.registerListener(this, sensor, SensorManager.SENSOR_DELAY_NORMAL)
        }
    }

    override fun onPause() {
        super.onPause()
        sensorManager.unregisterListener(this)
    }

    override fun onSensorChanged(event: SensorEvent?) {
        event?.let {
            if (it.sensor.type == Sensor.TYPE_ROTATION_VECTOR) {
                val rotationMatrix = FloatArray(9)
                SensorManager.getRotationMatrixFromVector(rotationMatrix, it.values)
                val orientation = FloatArray(3)
                SensorManager.getOrientation(rotationMatrix, orientation)
                _yaw = Math.toDegrees(orientation[0].toDouble()).toFloat()
                _pitch = Math.toDegrees(orientation[1].toDouble()).toFloat()
                _roll = Math.toDegrees(orientation[2].toDouble()).toFloat()
                // update the time
                _time = System.nanoTime() - _time0
            }
        }
    }

    override fun onAccuracyChanged(sensor: Sensor?, accuracy: Int) {
        // Do nothing
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        sensorManager = getSystemService(SENSOR_SERVICE) as SensorManager
        rotationVectorSensor = sensorManager.getDefaultSensor(Sensor.TYPE_ROTATION_VECTOR)


        setContent {
            FirstApplicationTheme {
                Scaffold(modifier = Modifier.fillMaxSize()) { innerPadding ->
                    SensorDataDisplay(
                        yaw = _yaw,
                        pitch = _pitch,
                        roll = _roll,
                        t = _time,
                        modifier = Modifier.padding(innerPadding)
                    )

                }
            }
        }
    }
}


@Composable
fun SensorDataDisplay(
    yaw: Float, pitch: Float, roll: Float, t: Long, modifier: Modifier = Modifier
) {
    val yawData = remember { mutableStateListOf<Entry>() }
    val pitchData = remember { mutableStateListOf<Entry>() }
    val rollData = remember { mutableStateListOf<Entry>() }
    if (t > 0) {
        yawData.add(Entry(t * 1e-9f, yaw))
        pitchData.add(Entry(t * 1e-9f, pitch))
        rollData.add(Entry(t * 1e-9f, roll))
    }
    Column(modifier = modifier) {
        val context = LocalContext.current
        Text(
            text = "qchen2015@hotmail.com © 2024",
            modifier = Modifier
                .padding(6.dp)
                .fillMaxWidth(),
            textAlign = TextAlign.Center
        )
        // add a hyperlink to the author's website
        Text(
            text = "https://www.windtunnel.cn",
            modifier = Modifier
                .padding(6.dp)
                .fillMaxWidth()
                .clickable {
                    val intent = Intent(Intent.ACTION_VIEW, Uri.parse("https://www.windtunnel.cn/categories/jetpack/"))
                    context.startActivity(intent)
                },
            textAlign = TextAlign.Center,
            color = Color.Blue,
            style = TextStyle(textDecoration = TextDecoration.Underline)
        )


        Text(
            text = "Yaw  : %16.4f°\nPitch: %16.4f°\nRoll  : %16.4f°\nTime: %16.6fs"
                .format(yaw, pitch, roll, t * 1e-9),
            modifier = Modifier.padding(16.dp)
        )
        SensorChart(yawData, pitchData, rollData, modifier = Modifier.fillMaxSize())
        // add an about button to show author information

    }
}


@RequiresApi(Build.VERSION_CODES.O)
fun showFileNameDialog(
    context: Context, yawData: List<Entry>, pitchData: List<Entry>, rollData: List<Entry>
) {
    val editText = EditText(context).apply {
        setHint("Enter file name")
        // get date and time
        val currentDateTime = java.time.LocalDateTime.now()
        val formatter = java.time.format.DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
        setText(currentDateTime.format(formatter))
    }
    val dialog = AlertDialog.Builder(context).setTitle("Enter file name").setView(editText)
        .setPositiveButton("Save") { _, _ ->
            val fileName = editText.text.toString()
            if (fileName.isNotEmpty()) {
                saveDataToCsv(context, fileName, yawData, pitchData, rollData)
            } else {
                Toast.makeText(context, "File name cannot be empty", Toast.LENGTH_SHORT).show()
            }
        }.setNegativeButton("Cancel", null).create()
    dialog.show()
}

fun saveDataToCsv(
    context: Context,
    fileName: String,
    yawData: List<Entry>,
    pitchData: List<Entry>,
    rollData: List<Entry>
) {
    val file = File(context.getExternalFilesDir(null), "${fileName.trim()}.csv")
    FileWriter(file).use { writer ->
        writer.append("Time,Yaw,Pitch,Roll\n")
        for (i in yawData.indices) {
            writer.append("${yawData[i].x},${yawData[i].y},${pitchData[i].y},${rollData[i].y}\n")
        }
    }
    Toast.makeText(context, "Data saved to ${file.absolutePath}", Toast.LENGTH_SHORT).show()
}