package streamrec

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import spire.algebra._
import spire.implicits._
import spire.math._

object Examples {

  @tailrec
  def gcd(x: Long, y: Long): Long = if (y == 0L) x else gcd(y, x % y)

  def main(args: Array[String]) {
    // the natural numbers: 0, 1, 2, ...
    val nats: InfStream[Int] =
      Macros.infinite1[Int, Int](() => 0, _ + 1, n => n)

    // the fibonnacci sequence: 0, 1, 1, ...
    val fibs: InfStream[Long] = Macros.infinite2[Long, Long, Long](
      () => (0L, 1L),
      (x, y) => (y, x + y),
      (x, y) => x
    )

    // the non-negative rationals: 0, 1, 2, 1/2, ...
    val rats0: InfStream[Rational] = Macros.infinite3[Rational, Long, Long, Long](
      () => (1L, 0L, 1L),
      { (i: Long, num: Long, den: Long) =>
        var nn = num - 1L
        var dd = den + 1L
        while (nn > 0L && gcd(nn, dd) != 1L) {
          nn -= 1L
          dd += 1L
        }
        if (nn <= 0L)
          (i + 1L, i, 1L)
        else
          (i, nn, dd)
      },
      (i: Long, num: Long, den: Long) => Rational(num, den)
    )

    // the rationals: 0, 1, -1, 2, -2, 1/2, ...
    val rats: InfStream[Rational] = Macros.infinite2[Rational, Rational, Boolean](
      () => (Rational(0L, 1L), false),
      { (last: Rational, sign: Boolean) =>
        if (sign) {
          (-last, false)
        } else {
          var n = -last.numeratorAsLong - 1
          var d = last.denominatorAsLong + 1
          var next = Rational(n, d)
          while (n > 0L && next.numeratorAsLong != n) {
            n -= 1L
            d += 1L
            next = Rational(n, d)
          }
          if (n <= 0L) (Rational(n + d, 1L), true) else (next, true)
        }
      },
      (r, _) => r
    )

    // the prime numbers (naive)
    val primes: InfStream[Long] = Macros.infinite3[Long, Long, Long, ArrayBuffer[Long]](
      () => (2L, 3L, ArrayBuffer.empty[Long]),
      { (curr: Long, next: Long, arr: ArrayBuffer[Long]) =>
        val len = arr.length
        var n = next
        var i = 0
        val limit = math.sqrt(n) + 1L
        while (i < len) {
          val k = arr(i)
          if (k > limit) {
            i = len
          } else if (n % k == 0L) {
            i = 0
            n += 2L
          } else {
            i += 1
          }
        }
        arr += n
        (n, n + 2, arr)
      },
      (curr: Long, _: Long, _: ArrayBuffer[Long]) => curr
    )

    println("nats    %s %s" format (nats.nth(100), nats.stream.take(10).toList))
    println("fibs    %s %s" format (fibs.nth(100), fibs.stream.take(10).toList))
    println("rats0   %s %s" format (rats0.nth(100).toString, rats0.stream.take(10).toList))
    println("rats    %s %s" format (rats.nth(100).toString, rats.stream.take(10).toList))
    println("primes  %s %s" format (primes.nth(100).toString, primes.stream.take(10).toList))
  }
}
