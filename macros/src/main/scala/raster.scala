package streamrec

import language.experimental.macros
import scala.reflect.macros.Context
import spire.implicits._

case class Raster(cols: Int, rows: Int, data: Array[Int]) { lhs =>
  def unop(f: Int => Int): Raster = {
    val d1 = data
    val d2 = new Array[Int](d1.length)
    cfor(0)(_ < d1.length, _ + 1) { i => d2(i) = f(d1(i)) }
    Raster(cols, rows, d2)
  }

  def binop(rhs: Raster)(f: (Int, Int) => Int): Raster = {
    val d1 = lhs.data
    val d2 = rhs.data
    val d3 = new Array[Int](d1.length)
    cfor(0)(_ < d1.length, _ + 1) { i => d3(i) = f(d1(i), d2(i)) }
    Raster(cols, rows, d3)
  }
}

object RasterOps {
  def unop(ct: Context)(r: ct.Expr[Raster])(f: ct.Expr[Int => Int]): ct.Expr[Raster] = {
    import ct.universe._
    import definitions._
    val util = new Macros.Util[ct.type](ct)
    util.verifyAnonymousFunction(f.tree)
    val List(raster, d1, d2, i) = util.names("raster", "d1", "d2", "i")

    val tree = q"""{
      val $raster = $r
      val $d1 = $raster.data
      val $d2 = new Array[Int]($d1.length)
      var $i = 0
      while ($i < $d1.length) {
        $d2($i) = $f($d1($i))
        $i += 1
      }
      Raster($raster.cols, $raster.rows, $d2)
    }"""

    util.inlineCleanupAndReset[Raster](tree)
  }

  def binop(ct: Context)(r1: ct.Expr[Raster], r2: ct.Expr[Raster])(f: ct.Expr[(Int, Int) => Int]): ct.Expr[Raster] = {
    import ct.universe._
    import definitions._
    val util = new Macros.Util[ct.type](ct)
    util.verifyAnonymousFunction(f.tree)
    val List(raster1, raster2, d1, d2, d3, i) =
      util.names("raster1", "raster2", "d1", "d2", "d3", "i")

    val tree = q"""{
      val $raster1 = $r1
      val $raster2 = $r2
      val $d1 = $raster1.data
      val $d2 = $raster2.data
      val $d3 = new Array[Int]($d1.length)
      var $i = 0
      while ($i < $d1.length) {
        $d3($i) = $f($d1($i), $d2($i))
        $i += 1
      }
      Raster($raster1.cols, $raster1.rows, $d3)
    }"""

    util.inlineCleanupAndReset[Raster](tree)
  }
}
