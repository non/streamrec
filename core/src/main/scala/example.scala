package streamrec

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, PriorityQueue}

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax._

object Examples {

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
  val rats1: InfStream[Rational] = Macros.infinite2[Rational, Rational, Boolean](
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
  val primes0: InfStream[Long] = Macros.infinite3[Long, Long, Long, ArrayBuffer[Long]](
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

  /**
   * Segmented Stream of Eratosthenes implementation
   * 
   * This section really needs some good comments.
   * 
   * Some future optimizations:
   * 
   * 1. Faster heap/priority queue
   * 2. Separate queue into fast array and one or more slow queues
   * 3. Tune chunkSize
   * 4. Consider trying a bitset for larger chunks
   * 5. Use Long internally until we have to switch to SafeLong.
   * 
   * Obviously InfStream has to be a bit more flexible than a
   * traditional prime finder that knows ahead of time what range it
   * will be operating over.
   */
  type N = SafeLong

  object N {
    def apply(n: Int) = SafeLong(n)
    def apply(n: Long) = SafeLong(n)
  }
  
  case class Factor(p: N, next: N) extends Ordered[Factor] {
    def compare(that: Factor): Int = -(this.next compare that.next)
  }

  case class Sieve(start: N, primes: Array[Boolean]) {
    def isPrime(n: N): Boolean = primes((n - start).toInt)
    def isComposite(n: N): Boolean = !primes((n - start).toInt)
    def set(n: N): Unit = primes((n - start).toInt) = true
    def unset(n: N): Unit = primes((n - start).toInt) = false

    def nextAfter(n: N): N = {
      var i = (n - start + 2).toInt
      while (i < primes.length) {
        if (primes(i)) return start + i
        i += 2
      }
      null
    }

    def init(fastq: PriorityQueue[Factor]) {
      initMod30()
      initFromQueue(start + primes.length, fastq)
      if (start == 0)
        initFirst(fastq)
      else
        initRest(fastq)
    }

    def initMod30() {
      var i: Int = 0
      while (i < primes.length) {
        primes(i + 1) = true
        primes(i + 7) = true
        primes(i + 11) = true
        primes(i + 13) = true
        primes(i + 17) = true
        primes(i + 19) = true
        primes(i + 23) = true
        primes(i + 29) = true
        i += 30
      }

      if (start == 0L) {
        primes(1) = false
        primes(2) = true
        primes(3) = true
        primes(5) = true
      }
    }

    @tailrec
    private def initFromQueue(limit: SafeLong, q: PriorityQueue[Factor]) {
      val factor = q.dequeue
      val m = factor.next
      if (m < limit) {
        val p = factor.p
        var i = (m - start).toInt
        val m2 = if (p < primes.length) {
          val k = p.toInt
          while (i < primes.length) { primes(i) = false; i += k }
          start + i
        } else {
          primes(i) = false
          m + p
        }
        q += Factor(p, m2)
        initFromQueue(limit, q)
      } else {
        q += factor
      }
    }

    def initFirst(q: PriorityQueue[Factor]) {
      var p: Int = 1
      while (p < primes.length) {
        if (primes(p)) {
          var m = p.toLong * p.toLong
          if (m < primes.length) {
            var k = m.toInt
            while (k < primes.length) { primes(k) = false; k += p }
            m = k.toLong
          }
          q += Factor(N(p), N(m))
        }
        p += 2
      }
    }

    def initRest(q: PriorityQueue[Factor]) {
      var i: Int = 1
      while (i < primes.length) {
        if (primes(i)) {
          val p: N = start + i
          q += Factor(p, p ** 2)
        }
        i += 2
      }
    }
  }

  case class Siever(chunkSize: Int) {
    if(chunkSize % 30 != 0) sys.error("chunkSize must be a multiple of 30")

    val arr = new Array[Boolean](chunkSize)
    var start: SafeLong = N(0)
    var limit: SafeLong = start + chunkSize
    val q: PriorityQueue[Factor] = PriorityQueue(Factor(N(2), N(4)))
    var sieve: Sieve = Sieve(start, arr)
    sieve.init(q)

    def nextAfter(n: N): N = {
      var nn = sieve.nextAfter(n)
      while (nn == null) {
        start += chunkSize
        limit += chunkSize
        var i = 0
        while (i < arr.length) { arr(i) = false; i += 1 }
        sieve = Sieve(start, arr)
        sieve.init(q)
        nn = sieve.nextAfter(start - 1)
      }
      nn
    }
  }

  val sieveSize = 900 * 1000

  val primes1: InfStream[N] = Macros.infinite3[N, Siever, N, N](
    () => (Siever(300000), N(2), N(3)),
    { (siever, a, b) =>
      val c = siever.nextAfter(b)
      (siever, b, c)
    },
    { (siever, a, b) => a }
  )

  def timer[A](s: String)(f: => A): A = {
    val t0 = System.nanoTime
    val a = f
    val t = System.nanoTime - t0
    println("%s generated %s in %.1f ms" format (s, a, t / 1000000.0))
    a
  }

  def main(args: Array[String]) {

    //println("timing the segmented sieve implementation")
    //List(100, 1000, 10000, 100000, 1000000, 10000000, 100000000).foreach { n =>
    //   timer("  primes1.nth(%s)" format n)(primes1.nth(n))
    // }

    println("comparing naive (0) and sieve (1) for nth prime:")
    List(100, 1000, 10000, 100000, 1000000).foreach { n =>
      timer("  primes0.nth(%s)" format n)(primes0.nth(n))
      timer("  primes1.nth(%s)" format n)(primes1.nth(n))
    }

    println("finding th 100th element, and the first 10 elements:")
    println("  nats    %s %s" format (nats.nth(100), nats.stream.take(10).toList))
    println("  fibs    %s %s" format (fibs.nth(100), fibs.stream.take(10).toList))
    println("  rats0   %s %s" format (rats0.nth(100).toString, rats0.stream.take(10).toList))
    println("  rats1   %s %s" format (rats1.nth(100).toString, rats1.stream.take(10).toList))
    println("  primes0  %s %s" format (primes0.nth(100).toString, primes0.stream.take(10).toList))
    println("  primes1  %s %s" format (primes1.nth(100).toString, primes1.stream.take(10).toList))
  }
}
