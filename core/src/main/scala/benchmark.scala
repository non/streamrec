package streamrec

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._
import scala.collection.mutable

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax._

object PrimeBenchmarks extends MyRunner(classOf[PrimeBenchmarks])

class PrimeBenchmarks extends MyBenchmark {
  //@Param(Array("10", "12", "14", "16", "18", "20", "22", "24", "26"))
  @Param(Array("17", "18", "19", "20"))
  var pow: Int = 0
  var n: Int = 0

  val sieveSize = 450 * 1000
  val cutoff = SafeLong(10000)

  override protected def setUp() {
    n = scala.math.pow(2, pow).toInt
  }

  def timeNth(reps: Int) = run(reps) {
    Siever(sieveSize, cutoff).nth(n)
  }
}

/**
 * Objects extending Runner will inherit a Caliper-compatible main method.
 */
abstract class MyRunner(cls:java.lang.Class[_ <: Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

/**
 * Extend Benchmark to gain some nice convenience methods.
 */
trait MyBenchmark extends SimpleBenchmark {
  // Sugar for building arrays using a per-cell init function.
  def init[A:ClassTag](size:Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  // Sugar to run 'f' for 'reps' number of times.
  def run(reps:Int)(f: => Unit) = for(i <- 0 until reps)(f)
}
