package streamrec

import spire.implicits._

case object Runtime

trait Op[A] {
  def run(sys: Runtime): A
}

case class Map1[A, B](a: Op[A])(f: A => B) extends Op[B] {
  def run(sys: Runtime): B = f(a.run(sys))
}

case class Map2[A, B, C](a: Op[A], b: Op[B])(f: (A, B) => C) extends Op[C] {
  def run(sys: Runtime): C = f(a.run(sys), b.run(sys))
}

object AddF {
  def apply(r1: Op[Raster], r2: Op[Raster]): Op[Raster] =
    Map2(r1, r2)((r1, r2) => r1.binop(r2)(_ + _))
}

// object AddM {
//   def apply(r1: Op[Raster], r2: Op[Raster]): Op[Raster] =
//     Map2(r1, r2)((r1, r2) => RasterOps.binop(r1, r2)(_ + _))
// }
