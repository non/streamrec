package streamrec

import language.experimental.macros
import scala.reflect.macros.Context
import spire.implicits._

object RasterOps {
  type R = Array[Int]

  def unop(ct: Context)(r: ct.Expr[R])(f: ct.Expr[Int => Int]): ct.Expr[R] = {
    import ct.universe._
    import definitions._
    val util = new Macros.Util[ct.type](ct)
    util.verifyAnonymousFunction(f.tree)
    val tree = q"""{
      val d = $r
      val out = new Array[Int](d.length)
      var i = 0
      while (i < d.length) {
        out(i) = $f(d(i))
        i += 1
      }
      out
    }"""

    util.inlineCleanupAndReset[R](tree)
  }

  def binop(ct: Context)(r1: ct.Expr[R], r2: ct.Expr[R])(f: ct.Expr[(Int, Int) => Int]): ct.Expr[R] = {
    import ct.universe._
    import definitions._
    val util = new Macros.Util[ct.type](ct)
    util.verifyAnonymousFunction(f.tree)
    val tree = q"""{
      val d1 = $r1
      val d2 = $r2
      val d3 = new Array[Int](d1.length)
      var i = 0
      while (i < d1.length) {
        d3(i) = $f(d1(i), d2(i))
        i += 1
      }
      d3
    }"""

    util.inlineCleanupAndReset[R](tree)
  }
}
