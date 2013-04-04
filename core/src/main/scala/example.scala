package streamrec

import scala.annotation.tailrec

object Examples {

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

  def main(args: Array[String]) {
    val nats: InfStream[Int] =
      Macros.infinite1[Int, Int](0, _ + 1, n => n)

    val fibs: InfStream[Long] =
      Macros.infinite2[Long, Long, Long]((0L, 1L), (b, c) => (c, b + c), (b, c) => b)

    val rats: InfStream[Double] =
      Macros.infinite3[Double, Long, Long, Long]((2L, 1L, 1L), { (z: Long, c: Long, d: Long) =>
        var nn = c - 1L
        var dd = d + 1L
        while (nn != 0L && gcd(nn, dd) != 1L) {
          nn -= 1L
          dd += 1L
        }
        if (nn == 0L) (z + 1L, z, 1L) else (z, nn, dd)
      }, (z: Long, c: Long, d: Long) => c.toDouble / d)

    println(nats.nth(100))
    println(nats.stream.take(10).toList)

    println(fibs.nth(10))
    println(fibs.stream.take(10).toList)

    println(rats.nth(10))
    println(rats.stream.take(10).toList)
  }
}
