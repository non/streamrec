package streamrec

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._
import scala.collection.mutable

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax._

object XyzBenchmarks extends MyRunner(classOf[XyzBenchmarks])

class XyzBenchmarks extends MyBenchmark {
  type R = Array[Int]

  @Param(Array("10000", "100000", "1000000"))
  //@Param(Array("100000", "1000000"))
  var size: Int = 0

  var a: R = _
  var b: R = _

  override protected def setUp() {
    a = init(size)(nextInt)
    b = init(size)(nextInt)
  }

  // def geniter(a: R, b: R, f: (Int, Int) => Int): R = {
  //   val out = new R(a.length)
  //   var i = 0
  //   while (i < out.length) {
  //     out(i) = f(a(i), b(i))
  //     i += 1
  //   }
  //   out
  // }
  // 
  // def genmin(a: R, b: R): R = geniter(a, b, Math.min)
  // def genmax(a: R, b: R): R = geniter(a, b, Math.max)
  // def genadd(a: R, b: R): R = geniter(a, b, _ + _)
  // def genmul(a: R, b: R): R = geniter(a, b, _ * _)
  // 
  // def dirmin(a: R, b: R): R = {
  //   val out = new R(a.length)
  //   var i = 0
  //   while (i < out.length) { out(i) = Math.min(a(i), b(i)); i += 1 }
  //   out
  // }
  // def dirmax(a: R, b: R): R = {
  //   val out = new R(a.length)
  //   var i = 0
  //   while (i < out.length) { out(i) = Math.max(a(i), b(i)); i += 1 }
  //   out
  // }
  // def diradd(a: R, b: R): R = {
  //   val out = new R(a.length)
  //   var i = 0
  //   while (i < out.length) { out(i) = a(i) + b(i); i += 1 }
  //   out
  // }
  // def dirmul(a: R, b: R): R = {
  //   val out = new R(a.length)
  //   var i = 0
  //   while (i < out.length) { out(i) = a(i) * b(i); i += 1 }
  //   out
  // }

  import Raster._

  // def timeGeneric1(reps: Int) = run(reps) { genadd(a, b) }
  // def timeDirect1(reps: Int) = run(reps) { diradd(a, b) }
  // def timeMacro1(reps: Int) = run(reps) { macadd(a, b) }
  // 
  // def timeGeneric2(reps: Int) = run(reps) { genadd(a, genmul(a, b)) }
  // def timeDirect2(reps: Int) = run(reps) { diradd(a, dirmul(a, b)) }
  // def timeMacro2(reps: Int) = run(reps) { macadd(a, macmul(a, b)) }

  def timeGeneric(reps: Int) = run(reps) { runGeneric }
  def runGeneric = {
    val c = genBinop(a, b)(_ + _)
    val d = genBinop(a, b)((a, b) => (a - b) * 10)
    val e = genBinop(a, b)((a, b) => Math.sqrt(a * a + b * b).toInt)
    val f = genBinop(a, b)((a, b) => Math.min(a, b))
    (c, d, e, f)
  }

  def timeMacro(reps: Int) = run(reps) { runMacro }
  def runMacro = {
    val c = macBinop(a, b)(_ + _)
    val d = macBinop(a, b)((a, b) => (a - b) * 10)
    val e = macBinop(a, b)((a, b) => Math.sqrt(a * a + b * b).toInt)
    val f = macBinop(a, b)((a, b) => Math.min(a, b))
    (c, d, e, f)
  }

  //def timeDirect(reps: Int) = run(reps) { dirsub(diradd(a, b), dirmul(a, b)) }

  // def timeGeneric5(reps: Int) = run(reps) { genmin(gensub(genadd(a, b), genmul(a, b)), genmul(a, b)) }
  // def timeDirect5(reps: Int) = run(reps) { dirmin(dirsub(diradd(a, b), dirmul(a, b)), dirmul(a, b)) }
  // def timeMacro5(reps: Int) = run(reps) { macmin(macsub(macadd(a, b), macmul(a, b)), macmul(a, b)) }
}
