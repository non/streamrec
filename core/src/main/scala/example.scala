package streamrec

import scala.annotation.tailrec

object Examples {
  // FIXME: verify that we're getting anonymous functions
  // FIXME: rewrite anonymous functions to use our names (b, c, ...)

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

  def main(args: Array[String]) {
    val nats: InfStream[Int] =
      Macros.infinite1[Int, Int](0, _ + 1, n => n)

    def blah(b: Int, c: Int): Int = b
    val fibs: InfStream[Int] =
      Macros.infinite2[Int, Int, Int]((0, 1), (b, c) => (c, b + c), blah)
    // val fibs: InfStream[Int] =
    //   Macros.infinite2[Int, Int, Int]((0, 1), (b, c) => (c, b + c), (b, c) => b)

    val rats: InfStream[Double] =
      Macros.infinite3[Double, Long, Long, Long]((2L, 1L, 1L), { (b: Long, c: Long, d: Long) =>
        var nn = c - 1L
        var dd = d + 1L
        while (nn != 0L && gcd(nn, dd) != 1L) {
          nn -= 1L
          dd += 1L
        }
        val (b2, c2, d2) = if (nn == 0L)
          (b + 1L, b, 1L)
        else
          (b, nn, dd)
        (b2, c2, d2)
      }, (b: Long, c: Long, d: Long) => c.toDouble / d)

    println(nats.nth(100))
    println(nats.stream.take(10).toList)

    println(fibs.nth(10))
    println(fibs.stream.take(10).toList)

    println(rats.nth(10))
    println(rats.stream.take(10).toList)
  }
}

trait InfStream1[A, B] extends InfStream[A] {
  def start: B
  def step(b: B): B
  def result(b: B): A

  def nth(n: Int): A = {
    @tailrec def loop(i: Int, b: B): A =
      if (i < 1) {
        result(b)
      } else {
        val b2 = step(b)
        loop(i - 1, b2)
      }
    val b = start
    loop(n, b)
  }

  def stream: Stream[A] = {
    def next(b: B): Stream[A] =
      result(b) #:: {
        val b2 = step(b)
        next(b2)
      }
    val b = start
    next(b)
  }
}

trait InfStream2[A, B, C] extends InfStream[A] {
  def start: (B, C)
  def step(b: B, c: C): (B, C)
  def result(b: B, c: C): A

  def nth(n: Int): A = {
    @tailrec def loop(i: Int, b: B, c: C): A =
      if (i < 1) {
        result(b, c)
      } else {
        val (b2, c2) = step(b, c)
        loop(i - 1, b2, c2)
      }
    val (b, c) = start
    loop(n, b, c)
  }

  def stream: Stream[A] = {
    def next(b: B, c: C): Stream[A] =
      result(b, c) #:: {
        val (b2, c2) = step(b, c)
        next(b2, c2)
      }
    val (b, c) = start
    next(b, c)
  }
}
