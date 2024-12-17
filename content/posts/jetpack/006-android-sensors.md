+++
title = 'Jetpack Compose for Android-006 传感器数据'
date = 2024-12-17T12:39:47+08:00
draft = false
mathjax = false
categories = ['jetpack', 'android']
tags = ['jetpack', 'compose', 'android', 'sensor']
toc = true
tocBorder = true
+++


![](/jetpack-imgs/001/spacerobot-yawpitchroll.gif)

## 需求分析

想要看看手机的传感器数据，看看滤波一下能玩点什么无聊的。先搞个最简单的，手机本身的姿态。

需求：采集手机姿态数据，显示在界面上。

那么我们需要：

- 一个文本标签类似的控件，显示手机姿态数据，三个角度：pitch, roll, yaw
- 是不是需要做一个图标？显示姿态的变化？
- 这样就提出了需要一个时间标签，显示采集数据的时间（间隔）
- 开始/停止采集数据的按钮是否需要？在这个场景，单一功能，不需要，把软件打开和软件关闭作为采集数据的开始和停止。
- 数据如何导出？肯定是需要的，那么我们考虑导出csv文件。

### 核心数据

- 时间序列，(t, pitch, roll, yaw)
- 采集间隔，$dt$，由硬件确定？

### 用户交互

- 打开程序
- 关闭程序
- 导出数据

### 界面设计

大概我们可以在上方设置一个标签，显示实时得到的最新数据，下方主体部分一个图标，动态更新，显示姿态的变化。

## 实现流程

### 建立工程

打开Androi的Studio，新建一个项目，选择Jetpack Compose模板。

![](/jetpack-imgs/001/activity.png)

记得要认准这个中间的`Compose`图标。

然后否就是一顿修改镜像地址。首先是gradle下载地址，修改`gradle/wrapper/gradle-wrapper.properties`文件：

```
{{% codesnap "static/jetpack-code/001/yawpitchroll/gradle/wrapper/gradle-wrapper.properties"  %}}
```

接下来就是修改`settings.gradle.kts`文件，增加下载地址：

```kotlin
{{% codesnap "static/jetpack-code/001/yawpitchroll/settings.gradle.kts"  %}}
```

只有经过了上面两步，才能什么同步Gradle 工程之类的，然后build一下，确认所有的依赖都下载完了。可以稍微运行一下也没问题。

### 建立界面

建立界面在Jetpack中间很简单很直观。

```kotlin
{{% codesnap "static/jetpack-code/001/yawpitchroll/app/src/main/java/org/cardc/fdii/qc/Instruments/MainActivity.kt"  %}}
```

这里面自己写的代码几乎没有，就是把`MainActivity`增加了一个继承`SensorEventListener`的接口，然后增加了一个`SensorManager`的实例，传感器`Sensor`实例，还有三个角度的数据、时间零点和当前时间。

`SensorEventListener`的接口要求实现几个方法：

- `onResume`，注册传感器监听器
- `onPause`，取消注册传感器监听器
- `onSensorChanged`，传感器数据变化时调用
- `onAccuracyChanged`，传感器精度变化时调用，这里我们不关心

在`MainActivity`的`onCreate`方法中，我们初始化了传感器管理和传感器实例。在`setContent`中，我们在`Scaffold`中增加了一个`SensorDataDisplay`的组件，这个组件是我们自己写的，用来显示传感器数据。

在这个`SensorDataDisplay`组件中，我们组织了一个`Column`，整个都是简单直观。

对于组件的输入变量，我们采用了`remember`的方式，这样可以在组件内部保存状态。当更新组件角度时，奖结果存入`mutableStateListOf<Entry>`中，这个`Entry`是`MPAndroidChart`库中的数据结构，用来存储图表数据。

第一行是一个版权信息，第二行稍微有一点意思，是一个可以点击的`Text`，会访问本站。

```kotlin
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
```

Android这一点就挺好，只要用`Intent`就可以打开浏览器，不用自己写什么复杂的东西。

第三行就是角度标签：

```kotlin
    Text(
        text = "Yaw  : %16.4f°\nPitch: %16.4f°\nRoll  : %16.4f°\nTime: %16.6fs"
            .format(yaw, pitch, roll, t * 1e-9),
        modifier = Modifier.padding(16.dp)
    )
```

第四行，是一个采用开源图标库`MPAndroidChart`的`LineChart`来实现的`SensorChart`，用来显示角度变化。

```kotlin
    SensorChart(yawData, pitchData, rollData, modifier = Modifier.fillMaxSize())
```

```kotlin
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
```

这里调用的是一个`AndroidView`，这个是Compose中的一个组件，用来显示Android原生的View。

这里实现一个动作，双击图表，会弹出一个对话框，让用户输入文件名，然后导出数据。

```kotlin
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
```

## 结论

导出的数据很容易用Matlab或者Python画出来。

![](/jetpack-imgs/001/log.png)

总的来说，这个过程非常丝滑，最终编译的`apk`文件大小不到10MB，非常适合用来搞一些无聊的事情。

- [代码](/jetpack-code/001/yawpitchroll.zip)
- [数据](/jetpack-code/001/20241214085714.csv)
- [apk不推荐下载](/jetpack-code/001/yawpitchroll.apk)