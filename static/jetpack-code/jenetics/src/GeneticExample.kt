import io.jenetics.*
import io.jenetics.engine.Codecs
import io.jenetics.engine.Engine
import io.jenetics.engine.EvolutionResult.toBestPhenotype
import io.jenetics.engine.EvolutionStatistics
import io.jenetics.engine.Limits.bySteadyFitness
import io.jenetics.util.DoubleRange





fun jeneticsExample(fn: (Double) -> Double, lb: Double, ub:Double): Phenotype<DoubleGene, Double>? {
    val engine: Engine<DoubleGene, Double> =
        Engine.builder(fn, Codecs.ofScalar(DoubleRange.of(lb, ub))).populationSize(20)
            .optimize(Optimize.MINIMUM).alterers(
                UniformCrossover(0.5), Mutator(0.03), MeanAlterer(0.6)
            ).build()

    val statistics = EvolutionStatistics.ofNumber<Double>()

    val best = engine.stream().limit(bySteadyFitness(10)).limit(100)
        // println the best phenotype after each generation
        .peek { print(it.generation()); print("\t"); println(it.bestPhenotype()) }.peek(statistics)
        .collect(toBestPhenotype())

    println(statistics)
    println(best)


    return best
}