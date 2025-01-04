import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.math.cos
import kotlin.math.sin

class DeriveTest {
    val epsilon = 1e-9
    val hD = 1e-3

    @Test
    fun testSimpleDerive() {
        fun f(x: Double): Double {
            return x * x
        }

        val x = 2.0
        val dfs = FunctionPointAndDerivatives.apply { h = hD }.of(x, ::f)
        assertEquals(x, dfs.point, epsilon)
        assertEquals(x * x, dfs.fitness, epsilon)
        assertEquals(2.0 * x, dfs.firstDerivative, epsilon)
        assertEquals(2.0, dfs.secondDerivative, epsilon)
    }

    @Test
    fun testSinDerive() {
        val theta = 0.5
        val dfs = FunctionPointAndDerivatives.apply { h = hD }.of(theta) {
            sin(it)
        }

        assertEquals(theta, dfs.point, epsilon)
        assertEquals(sin(theta), dfs.fitness, epsilon)
        assertEquals(cos(theta), dfs.firstDerivative, epsilon)
        assertEquals(-sin(theta), dfs.secondDerivative, epsilon)
    }

    @Test
    fun testCosDerive() {
        val theta = 0.5
        val dfs = FunctionPointAndDerivatives.apply { h = hD }.of(theta) {
            cos(it)
        }

        assertEquals(theta, dfs.point, epsilon)
        assertEquals(cos(theta), dfs.fitness, epsilon)
        assertEquals(-sin(theta), dfs.firstDerivative, epsilon)
        assertEquals(-cos(theta), dfs.secondDerivative, epsilon)
    }

    @Test
    fun testSinPolyDerive() {
        val theta = 0.5
        val dfs = FunctionPointAndDerivatives.apply { h = hD }.of(theta) {
            sin(it) * it * it
        }

        assertEquals(theta, dfs.point, epsilon)
        assertEquals(sin(theta) * theta * theta, dfs.fitness, epsilon)
        assertEquals(
            2 * sin(theta) * theta + cos(theta) * theta * theta,
            dfs.firstDerivative,
            epsilon
        )
        assertEquals(
            2 * cos(theta) * theta + 2 * sin(theta) - sin(theta) * theta * theta + 2 * cos(theta) * theta,
            dfs.secondDerivative,
            epsilon
        )
    }
}