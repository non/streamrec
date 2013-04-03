package streamrec

import language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.tailrec

trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}

object Macros {
  def infinite1[A, B](start: B, step: B => B, result: B => A): InfStream[A] =
    macro inf1[A, B]

  def infinite2[A, B, C](start: (B, C), step: (B, C) => (B, C), result: (B, C) => A): InfStream[A] =
    macro inf2[A, B, C]

  def infinite3[A, B, C, D](start: (B, C, D), step: (B, C, D) => (B, C, D), result: (B, C, D) => A): InfStream[A] =
    macro inf3[A, B, C, D]

  def inf1[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)
    (start: c.Expr[B], step: c.Expr[B => B], result: c.Expr[B => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val expr = reify {
      new InfStream[A] {
        def nth(n: Int): A = {
          @tailrec def loop(i: Int, b: B): A =
            if (i < 1) {
              result.splice.apply(b)
            } else {
              val b2 = step.splice.apply(b)
              loop(i - 1, b2)
            }
          val b = start.splice
          loop(n, b)
        }

        def stream: Stream[A] = {
          def next(b: B): Stream[A] =
            result.splice.apply(b) #:: {
              val b2 = step.splice.apply(b)
              next(b2)
            }
          val b = start.splice
          next(b)
        }
      }
    }

    val out = new Util[c.type](c).inlineAndReset[InfStream[A]](expr.tree)
    //println(show(out))
    out
  }

  def inf2[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag](c: Context)
    (start: c.Expr[(B, C)], step: c.Expr[(B, C) => (B, C)], result: c.Expr[(B, C) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val expr = reify {
      new InfStream[A] {
        def nth(n: Int): A = {
          @tailrec def loop(i: Int, b: B, c: C): A =
            if (i < 1) {
              result.splice.apply(b, c)
            } else {
              val (b2, c2) = step.splice.apply(b, c)
              loop(i - 1, b2, c2)
            }
          val (b, c) = start.splice
          loop(n, b, c)
        }

        def stream: Stream[A] = {
          def next(b: B, c: C): Stream[A] =
            result.splice.apply(b, c) #:: {
              val (b2, c2) = step.splice.apply(b, c)
              next(b2, c2)
            }
          val (b, c) = start.splice
          next(b, c)
        }
      }
    }

    val out = new Util[c.type](c).inlineAndReset[InfStream[A]](expr.tree)
    //println(show(out))
    out
  }

  def inf3[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, D: c.WeakTypeTag](c: Context)
    (start: c.Expr[(B, C, D)], step: c.Expr[(B, C, D) => (B, C, D)], result: c.Expr[(B, C, D) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val expr = reify {
      new InfStream[A] {
        def nth(n: Int): A = {
          @tailrec def loop(i: Int, b: B, c: C, d: D): A =
            if (i < 1) {
              result.splice.apply(b, c, d)
            } else {
              val (b2, c2, d2) = step.splice.apply(b, c, d)
              loop(i - 1, b2, c2, d2)
            }
          val (b, c, d) = start.splice
          loop(n, b, c, d)
        }

        def stream: Stream[A] = {
          def next(b: B, c: C, d: D): Stream[A] =
            result.splice.apply(b, c, d) #:: {
              val (b2, c2, d2) = step.splice.apply(b, c, d)
              next(b2, c2, d2)
            }
          val (b, c, d) = start.splice
          next(b, c, d)
        }
      }
    }

    val out = new Util[c.type](c).inlineAndReset[InfStream[A]](expr.tree)
    //println(show(out))
    out
  }

  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    object Fixer extends Transformer {
      def handle(trees: List[Tree]): List[Tree] = trees match {
        case (t @ ValDef(
          Modifiers(_), TermName(xx), _,
          Match(
            Annotated(Apply(Select(New(Ident(_)), _), Nil),
              Apply(TypeApply(Select(Select(Ident(scala), TermName("Tuple2")), TermName("apply")), _),
                List(arg1, arg2))), _))) ::
            (u @ ValDef(_, TermName(bb), _, Select(Ident(TermName(xx1)), TermName("_1")))) ::
            (v @ ValDef(_, TermName(cc), _, Select(Ident(TermName(xx2)), TermName("_2")))) ::
            Nil if xx == xx1 && xx == xx2 =>
          List(
            ValDef(Modifiers(), TermName(bb), TypeTree(), arg1),
            ValDef(Modifiers(), TermName(cc), TypeTree(), arg2)
          )

        case (t @ ValDef(
          Modifiers(_), TermName(xx), _,
          Match(
            Annotated(Apply(Select(New(Ident(_)), _), Nil),
              Apply(TypeApply(Select(Select(Ident(scala), TermName("Tuple3")), TermName("apply")), _),
                List(arg1, arg2, arg3))), _))) ::
            (u @ ValDef(_, TermName(bb), _, Select(Ident(TermName(xx1)), TermName("_1")))) ::
            (v @ ValDef(_, TermName(cc), _, Select(Ident(TermName(xx2)), TermName("_2")))) ::
            (w @ ValDef(_, TermName(dd), _, Select(Ident(TermName(xx3)), TermName("_3")))) ::
            Nil if xx == xx1 && xx == xx2 && xx == xx3 =>
          val xs = List(
            ValDef(Modifiers(), TermName(bb), TypeTree(), arg1),
            ValDef(Modifiers(), TermName(cc), TypeTree(), arg2),
            ValDef(Modifiers(), TermName(dd), TypeTree(), arg3)
          )
          println("yuussssss: %s" format xs)
          xs

        case t :: ts =>
          t :: handle(ts)

        case Nil =>
          Nil
      }

      override def transform(tree: Tree): Tree = tree match {
        case Block(trees, last) =>
          val b = Block(handle(trees), last)
          super.transform(b)
        case _ =>
          super.transform(tree)
      }
    }

    def fixup(tree: Tree): Tree = Fixer.transform(tree)

    def inlineAndReset[T](tree: Tree): c.Expr[T] =
      c.Expr[T](c.resetAllAttrs(fixup(inlineApplyRecursive(tree))))

    def inlineApplyRecursive(tree: Tree): Tree = {
      val ApplyName = TermName("apply")

      class InlineSymbol(symbol: Symbol, value: Tree) extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Ident(_) if tree.symbol == symbol => value
          case _ => super.transform(tree)
        }
      }

      object InlineApply extends Transformer {
        def inlineSymbol(symbol: Symbol, body: Tree, arg: Tree): Tree =
          new InlineSymbol(symbol, arg).transform(body)

        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(Function(params, body), ApplyName), args) =>
            params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
              inlineSymbol(param.symbol, body, arg)
            }
  
          case Apply(Function(params, body), args) =>
            params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
              inlineSymbol(param.symbol, body, arg)
            }
  
          case _ =>
            super.transform(tree)
        }
      }

      InlineApply.transform(tree)
    }
  }
}
