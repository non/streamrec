package streamrec

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._
import scala.collection.mutable
import scala.annotation.tailrec

import spire.algebra._
import spire.implicits._
import spire.math._

// object FibonacciBenchmark extends MyRunner(classOf[FibonacciBenchmark])
// class FibonacciBenchmark extends MyBenchmark {
//   val num = 40

//   def nthDirect(n: Int): SafeLong = {
//     @tailrec def loop(i: Int, x: SafeLong, y: SafeLong): SafeLong = {
//       if (i <= 1) x else loop(i - 1, y, x + y)
//     }
//     loop(n, SafeLong(0L), SafeLong(1L))
//   }

//   def nthIndirect(n: Int): SafeLong = {
//     @tailrec def loop(i: Int, b: (SafeLong, SafeLong)): SafeLong =
//       if (i <= 1) {
//         b._1
//       } else {
//         val (x, y) = b
//         loop(i - 1, (y, x + y))
//       }
//     loop(n, (SafeLong(0L), SafeLong(1L)))
//   }

//   val fibs = Macros.infinite2[SafeLong, SafeLong, SafeLong](
//     () => (SafeLong(0L), SafeLong(1L)),
//     (x, y) => (y, x + y),
//     (x, y) => x)

//   val fibsDirect = new InfStream[SafeLong] {
//     def nth(n: Int): SafeLong = {
//       @tailrec def loop(i: Int, b: SafeLong, c: SafeLong): SafeLong =
//         if (i <= 1) b else loop(i - 1, c, b + c)
//       loop(n, SafeLong(0L), SafeLong(1L))
//     }

//     def stream: Stream[SafeLong] = {
//       def next(b: SafeLong, c: SafeLong): Stream[SafeLong] = b #:: next(c, b + c)
//       next(SafeLong(0L), SafeLong(1L))
//     }
//   }

//   def timeNthDirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(SafeLong(0L))((t, _) => t + nthDirect(num))
//   }
//   def timeNthIndirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(SafeLong(0L))((t, _) => t + nthIndirect(num))
//   }
//   def timeNthInfStream(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(SafeLong(0L))((t, _) => t + fibs.nth(num))
//   }
//   def timeNthInfStreamDirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(SafeLong(0L))((t, _) => t + fibsDirect.nth(num))
//   }
// }

// object FibBenchmark extends MyRunner(classOf[FibBenchmark])
// class FibBenchmark extends MyBenchmark {
//   val num = 40

//   def nthDirect(n: Int): Long = {
//     @tailrec def loop(i: Int, x: Long, y: Long): Long = {
//       if (i <= 1) x else loop(i - 1, y, x + y)
//     }
//     loop(n, 0L, 1L)
//   }

//   def nthIndirect(n: Int): Long = {
//     @tailrec def loop(i: Int, b: (Long, Long)): Long =
//       if (i <= 1) {
//         b._1
//       } else {
//         val (x, y) = b
//         loop(i - 1, (y, x + y))
//       }
//     loop(n, (0L, 1L))
//   }

//   val fibs = Macros.infinite2[Long, Long, Long](
//     () => (0L, 1L),
//     (x, y) => (y, x + y),
//     (x, y) => x)

//   val fibsDirect = new InfStream[Long] {
//     def nth(n: Int): Long = {
//       @tailrec def loop(i: Int, b: Long, c: Long): Long =
//         if (i <= 1) b else loop(i - 1, c, b + c)
//       loop(n, 0L, 1L)
//     }

//     def stream: Stream[Long] = {
//       def next(b: Long, c: Long): Stream[Long] = b #:: next(c, b + c)
//       next(0L, 1L)
//     }
//   }

//   def timeNthDirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(0L)((t, _) => t + nthDirect(num))
//   }
//   def timeNthIndirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(0L)((t, _) => t + nthIndirect(num))
//   }
//   def timeNthInfStream(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(0L)((t, _) => t + fibs.nth(num))
//   }
//   def timeNthInfStreamDirect(reps: Int) = run(reps) {
//     (0 until 100).foldLeft(0L)((t, _) => t + fibsDirect.nth(num))
//   }
// }

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
