package streamrec

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._
import scala.collection.mutable
import scala.annotation.tailrec

import spire.algebra._
import spire.implicits._
import spire.math._

object FibonacciBenchmark extends MyRunner(classOf[FibonacciBenchmark])
class FibonacciBenchmark extends MyBenchmark {
  def nthDirect(n: Int): SafeLong = {
    @tailrec def loop(i: Int, x: SafeLong, y: SafeLong): SafeLong = {
      if (i <= 1) x else loop(i - 1, y, x + y)
    }
    loop(n, SafeLong(0L), SafeLong(1L))
  }

  def nthIndirect(n: Int): SafeLong = {
    @tailrec def loop(i: Int, b: (SafeLong, SafeLong)): SafeLong =
      if (i <= 1) {
        b._1
      } else {
        val (x, y) = b
        loop(i - 1, (y, x + y))
      }
    loop(n, (SafeLong(0L), SafeLong(1L)))
  }

  val fibs = Macros.infinite2[SafeLong, SafeLong, SafeLong](
    () => (SafeLong(0L), SafeLong(1L)),
    (x, y) => (y, x + y),
    (x, y) => x)

  val fibsDirect = new InfStream[SafeLong] {
    def nth(n: Int): SafeLong = {
      @tailrec def loop(i: Int, b: SafeLong, c: SafeLong): SafeLong =
        if (i <= 1) b else loop(i - 1, c, b + c)
      loop(n, SafeLong(0L), SafeLong(1L))
    }
    def stream: Stream[SafeLong] = ???
    def vector(n: Int): Vector[SafeLong] = ???
  }

  class IndirectInfStream[S, R](init: () => S, next: S => S, get: S => R) {
    def nth(n: Int): R = {
      @tailrec def loop(i: Int, s: S): R =
        if (i <= 1) get(s) else loop(i - 1, next(s))
      loop(n, init())
    }
    def stream: Stream[R] = ???
    def vector(n: Int): Vector[R] = ???
  }

  val fibsIndirect = new IndirectInfStream[(SafeLong, SafeLong), SafeLong](
    () => (SafeLong(0L), SafeLong(1L)),
    t => (t._2, t._1 + t._2),
    t => t._1)


  @Param(Array("50", "100", "200", "300"))
  var num: Int = 0

  def timeNthDirect(reps: Int) = run(reps) { nthDirect(num) }
  def timeNthIndirect(reps: Int) = run(reps) { nthIndirect(num) }
  def timeNthInfStream(reps: Int) = run(reps) { fibs.nth(num) }
  def timeNthInfStreamDirect(reps: Int) = run(reps) { fibsDirect.nth(num) }
  def timeNthInfStreamIndirect(reps: Int) = run(reps) { fibsIndirect.nth(num) }
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
