package streamrec

import scala.{specialized => spec}

import language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.tailrec
import scala.collection.mutable

trait InfStream[A] {
  def nth(n: Int): A
  def stream: scala.collection.immutable.Stream[A]
}

object Macros {
  def infinite1[A, B](start: () => B, step: B => B, result: B => A): InfStream[A] =
    macro inf1[A, B]

  def infinite2[A, B, C](start: () => (B, C), step: (B, C) => (B, C), result: (B, C) => A): InfStream[A] =
    macro inf2[A, B, C]

  def infinite3[A, B, C, D](start: () => (B, C, D), step: (B, C, D) => (B, C, D), result: (B, C, D) => A): InfStream[A] =
    macro inf3[A, B, C, D]

  def inf1[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)
    (start: c.Expr[() => B], step: c.Expr[B => B], result: c.Expr[B => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val util = new Util[c.type](c)
    val reserved = Set("__i", "__n", "__b", "__b2")

    util.verifyAnonymousFunction(start.tree, reserved)
    util.verifyAnonymousFunction(step.tree, reserved)
    util.verifyAnonymousFunction(result.tree, reserved)

    val expr = reify {
      new InfStream[A] {
        def nth(__n: Int): A = {
          @tailrec def loop(__i: Int, __b: B): A =
            if (__i <= 1) {
              result.splice.apply(__b)
            } else {
              val __b2 = step.splice.apply(__b)
              loop(__i - 1, __b2)
            }
          val __b = start.splice.apply
          loop(__n, __b)
        }

        def stream: scala.collection.immutable.Stream[A] = {
          def next(__b: B): scala.collection.immutable.Stream[A] =
            result.splice.apply(__b) #:: {
              val __b2 = step.splice.apply(__b)
              next(__b2)
            }
          val __b = start.splice.apply
          next(__b)
        }
      }
    }

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  def inf2[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag](c: Context)
    (start: c.Expr[() => (B, C)], step: c.Expr[(B, C) => (B, C)], result: c.Expr[(B, C) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val util = new Util[c.type](c)
    val reserved = Set("__i", "__n", "__b", "__b2", "__c", "__c2")

    util.verifyAnonymousFunction(start.tree, reserved)
    util.verifyAnonymousFunction(step.tree, reserved)
    util.verifyAnonymousFunction(result.tree, reserved)

    val expr = reify {
      new InfStream[A] {
        def nth(__n: Int): A = {
          @tailrec def loop(__i: Int, __b: B, __c: C): A =
            if (__i <= 1) {
              result.splice.apply(__b, __c)
            } else {
              val (__b2, __c2) = step.splice.apply(__b, __c)
              loop(__i - 1, __b2, __c2)
            }
          val (__b, __c) = start.splice.apply
          loop(__n, __b, __c)
        }

        def stream: scala.collection.immutable.Stream[A] = {
          def next(__b: B, __c: C): scala.collection.immutable.Stream[A] =
            result.splice.apply(__b, __c) #:: {
              val (__b2, __c2) = step.splice.apply(__b, __c)
              next(__b2, __c2)
            }
          val (__b, __c) = start.splice.apply
          next(__b, __c)
        }
      }
    }

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  def inf3[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, D: c.WeakTypeTag](c: Context)
    (start: c.Expr[() => (B, C, D)], step: c.Expr[(B, C, D) => (B, C, D)], result: c.Expr[(B, C, D) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val util = new Util[c.type](c)
    val reserved = Set("__i", "__n", "__b", "__b2", "__c", "__c2", "__d", "__d2")

    util.verifyAnonymousFunction(start.tree, reserved)
    util.verifyAnonymousFunction(step.tree, reserved)
    util.verifyAnonymousFunction(result.tree, reserved)

    val expr = reify {
      new InfStream[A] {
        def nth(__n: Int): A = {
          @tailrec def loop(__i: Int, __b: B, __c: C, __d: D): A =
            if (__i <= 1) {
              result.splice.apply(__b, __c, __d)
            } else {
              val (__b2, __c2, __d2) = step.splice.apply(__b, __c, __d)
              loop(__i - 1, __b2, __c2, __d2)
            }
          val (__b, __c, __d) = start.splice.apply
          loop(__n, __b, __c, __d)
        }

        def stream: scala.collection.immutable.Stream[A] = {
          def next(__b: B, __c: C, __d: D): scala.collection.immutable.Stream[A] =
            result.splice.apply(__b, __c, __d) #:: {
              val (__b2, __c2, __d2) = step.splice.apply(__b, __c, __d)
              next(__b2, __c2, __d2)
            }
          val (__b, __c, __d) = start.splice.apply
          next(__b, __c, __d)
        }
      }
    }

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  /**
   * The Util class supports inlining and related optimizations.
   */
  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    class CollisionChecker(names: Set[String]) extends Transformer {
      var seen = mutable.Set.empty[String]
      override def transform(tree: Tree): Tree = tree match {
        case t @ Ident(TermName(s)) if names(s) =>
          seen.add(s)
          t
        case _ =>
          super.transform(tree)
      }
      def check(tree: Tree) {
        transform(tree)
        if (!seen.isEmpty) {
          val names = seen.mkString(", ")
          val msg = s"Anonymous function using reserved names: $names"
          c.abort(c.enclosingPosition, msg)
        }
      }
    }

    def isAnonymousFunction(t: Tree): Boolean = t match {
      case Function(_, _) => true
      case _ => false
    }

    def verifyAnonymousFunction(t: Tree, reserved: Set[String]) {
      if (!isAnonymousFunction(t))
        c.abort(c.enclosingPosition, "Arguments required to be anonymous functions")
      else
        new CollisionChecker(reserved).check(t)
    }

    object TermName {
      def apply(s: String) = newTermName(s)
      def unapply(name: TermName) = Some(name.encoded)
    }

    val ApplyName = TermName("apply")

      /**
       * Given the following code:
       * 
       *   val t = Tuple6#apply(a0, b0, ...f0)
       *   val a = t._1
       *   val b = t._2
       *   ...
       *   val f = t._6
       * 
       * This transformer rewrites it to avoid tuple construction:
       * 
       *   val a = a0
       *   val b = b0
       *   ...
       *   val f = f0
       * 
       * It should work for tuples of any size.
       */
    object Fixer extends Transformer {

      /**
       * Check the given list of trees to see if they reprsent a series
       * of tuple access. Return either None (if they don't) or a Some
       * tuple of the argument names, and the remaining trees (that were
       * not part of the tuple assignments).
       */
      def matchTplDerefs(n: Int, tplName: String, trees: List[Tree]): Option[(List[String], List[Tree])] = {
        @tailrec
        def loop(i: Int, ns: List[String], ts: List[Tree]): Option[(List[String], List[Tree])] = {
          if (i < n) {
            val tplGetter = "_" + (i + 1).toString
            ts match {
              case Nil =>
                None
              case ValDef(_, TermName(s), _,
                Select(Ident(TermName(tplName)), TermName(`tplGetter`))) :: tail =>
                loop(i + 1, s :: ns, tail)
            }
          } else {
            Some((ns.reverse, ts))
          }
        }
        loop(0, Nil, trees)
      }

      def handle(trees: List[Tree]): List[Tree] = trees match {
        case ValDef(
          _: Modifiers, TermName(xx), _,
          Match(
            Annotated(Apply(Select(New(Ident(_)), _), Nil),
              Apply(TypeApply(Select(Select(Ident(scala), TermName(tn)), ApplyName), _),
                args)), _)) :: rest if tn.startsWith("Tuple") =>

          matchTplDerefs(args.length, tn, rest) match {
            case Some((names, rest)) =>
              names.zip(args).map { case (name, arg) =>
                ValDef(Modifiers(), TermName(name), TypeTree(), arg): Tree
              } ::: handle(rest)
            case None =>
              trees.head :: handle(rest)
          }

        case t :: ts =>
          t :: handle(ts)

        case Nil =>
          Nil
      }

      override def transform(tree: Tree): Tree = tree match {
        case Block(trees, last) =>
          super.transform(Block(handle(trees), last))

        case _ =>
          super.transform(tree)
      }
    }

    /**
     * This transformer is designed to clean up extra matching and
     * Typed detritus that may otherwise derail inlining.
     *
     * TODO: Figure out why inliner doesn't rewrite Typed nodes.
     * 
     * TODO: See if we need similar clean up for Tuple2.
     */
    object Cleanup extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Match(
          body,
          List(
            CaseDef(Apply(Ident(TermName("Tuple3")), List(
              Bind(TermName("b2"), Ident(nme.WILDCARD)),
              Bind(TermName("c2"), Ident(nme.WILDCARD)),
              Bind(TermName("d2"), Ident(nme.WILDCARD))
            )), EmptyTree,
              Apply(Select(Ident(TermName("Tuple3")), ApplyName), List(
                Ident(TermName("b2")),
                Ident(TermName("c2")),
                Ident(TermName("d2"))
              ))))) =>
          transform(body)

        case Match(
          body,
          List(
            CaseDef(Apply(Ident(TermName("Tuple2")), List(
              Bind(TermName("b2"), Ident(nme.WILDCARD)),
              Bind(TermName("c2"), Ident(nme.WILDCARD))
            )), EmptyTree,
              Apply(Select(Ident(TermName("Tuple2")), ApplyName), List(
                Ident(TermName("b2")),
                Ident(TermName("c2"))
              ))))) =>
          transform(body)

        case Typed(y, z) => y

        case _ =>
          super.transform(tree)
      }
    }

    def fixup(tree: Tree): Tree = Fixer.transform(tree)

    def inlineCleanupAndReset[T](tree: Tree): c.Expr[T] = {
      val inlined = inlineApplyRecursive(tree)
      val fixed = fixup(inlined)
      val cleaned = Cleanup.transform(fixed)
      c.Expr[T](c.resetAllAttrs(cleaned))
    }

    /**
     * Inling anonymous function application.
     * 
     * This means taking something like this:
     * 
     *   ((x: Int, y: Int) => x + y * 2).apply(3, 44)
     * 
     * And rewriting it to:
     * 
     *   (3 + 44 * 2)
     * 
     * We want to avoid any extra indirection, including creating
     * scopes or assigning intermediate vals.
     */
    def inlineApplyRecursive(tree: Tree): Tree = {

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
              inlineSymbol(param.symbol, b, arg)
            }
  
          case Apply(Function(params, body), args) =>
            params.zip(args).foldLeft(body) { case (b, (param, arg)) =>
              inlineSymbol(param.symbol, b, arg)
            }
  
          case _ =>
            super.transform(tree)
        }
      }

      InlineApply.transform(tree)
    }
  }
}
