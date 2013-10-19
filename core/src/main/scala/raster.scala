package streamrec

import spire.implicits._
import scala.language.experimental.macros

object Raster {
  type R = Array[Int]

  def genUnop(d: R)(f: Int => Int): R = {
    val out = new Array[Int](d.length)
    var i = 0
    while (i < d.length) {
      out(i) = f(d(i))
      i += 1
    }
    out
  }

  def genBinop(d1: R, d2: R)(f: (Int, Int) => Int): R = {
    val d3 = new Array[Int](d1.length)
    var i = 0
    while (i < d1.length) {
      d3(i) = f(d1(i), d2(i))
      i += 1
    }
    d3
  }

  def macUnop(r: R)(f: Int => Int): R = macro RasterOps.unop

  def macBinop(r1: R, r2: R)(f: (Int, Int) => Int): R = macro RasterOps.binop

  // def dirmin(d1: R, d2: R): R = {
  //   val d3 = new Array[Int](d1.length)
  //   var i = 0
  //   while (i < d1.length) { d3(i) = Math.min(d1(i), d2(i)); i += 1 }
  //   d3
  // }
  // 
  // def dirmax(d1: R, d2: R): R = {
  //   val d3 = new Array[Int](d1.length)
  //   var i = 0
  //   while (i < d1.length) { d3(i) = Math.max(d1(i), d2(i)); i += 1 }
  //   d3
  // }
  // 
  // def diradd(d1: R, d2: R): R = {
  //   val d3 = new Array[Int](d1.length)
  //   var i = 0
  //   while (i < d1.length) { d3(i) = d1(i) + d2(i); i += 1 }
  //   d3
  // }
  // 
  // def dirmul(d1: R, d2: R): R = {
  //   val d3 = new Array[Int](d1.length)
  //   var i = 0
  //   while (i < d1.length) { d3(i) = d1(i) * d2(i); i += 1 }
  //   d3
  // }
  // 
  // def dirsub(d1: R, d2: R): R = {
  //   val d3 = new Array[Int](d1.length)
  //   var i = 0
  //   while (i < d1.length) { d3(i) = d1(i) - d2(i); i += 1 }
  //   d3
  // }
}
