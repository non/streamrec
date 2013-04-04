package streamrec

import scala.annotation.tailrec

object Examples {

  @tailrec
  def gcd(x: Long, y: Long): Long = if (y == 0L) x else gcd(y, x % y)

  def main(args: Array[String]) {
    val nats: InfStream[Int] =
      Macros.infinite1[Int, Int](0, _ + 1, n => n)

    val fibs: InfStream[Long] =
      Macros.infinite2[Long, Long, Long]((0L, 1L), (x, y) => (y, x + y), (x, y) => x)

    val rats: InfStream[Double] =
      Macros.infinite3[Double, Long, Long, Long]((2L, 1L, 1L), { (i: Long, num: Long, den: Long) =>
        var nn = num - 1L
        var dd = den + 1L
        while (nn != 0L && gcd(nn, dd) != 1L) {
          nn -= 1L
          dd += 1L
        }
        if (nn == 0L) (i + 1L, i, 1L) else (i, num, den)
      }, (i: Long, num: Long, den: Long) => num.toDouble / den)

    println(nats.nth(100))
    println(nats.stream.take(10).toList)

    println(fibs.nth(10))
    println(fibs.stream.take(10).toList)

    println(rats.nth(10))
    println(rats.stream.take(10).toList)
  }
}
