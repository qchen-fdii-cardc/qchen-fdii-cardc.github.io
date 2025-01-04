import kotlin.math.cos
import kotlin.math.sin

fun fitness(d: Double): Double {
    return cos(0.5 + sin(d)) * cos(d)
}
