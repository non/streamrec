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

    val util = new Util[c.type](c)

    if (!util.isAnonymousFunction(step.tree))
      c.abort(c.enclosingPosition, "step parameter must be an anonymous function")
    if (!util.isAnonymousFunction(result.tree))
      c.abort(c.enclosingPosition, "result parameter must be an anonymous function")

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

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  def inf2[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag](c: Context)
    (start: c.Expr[(B, C)], step: c.Expr[(B, C) => (B, C)], result: c.Expr[(B, C) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val util = new Util[c.type](c)

    if (!util.isAnonymousFunction(step.tree))
      c.abort(c.enclosingPosition, "step parameter must be an anonymous function")
    if (!util.isAnonymousFunction(result.tree))
      c.abort(c.enclosingPosition, "result parameter must be an anonymous function")

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

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  def inf3[A: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, D: c.WeakTypeTag](c: Context)
    (start: c.Expr[(B, C, D)], step: c.Expr[(B, C, D) => (B, C, D)], result: c.Expr[(B, C, D) => A]): c.Expr[InfStream[A]] = {

    import c.universe._
    import definitions._

    val util = new Util[c.type](c)

    if (!util.isAnonymousFunction(step.tree))
      c.abort(c.enclosingPosition, "step parameter must be an anonymous function")
    if (!util.isAnonymousFunction(result.tree))
      c.abort(c.enclosingPosition, "result parameter must be an anonymous function")

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

    util.inlineCleanupAndReset[InfStream[A]](expr.tree)
  }

  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    def isAnonymousFunction(t: Tree): Boolean = t match {
      case Function(_, _) => true
      case _ => false
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
          Modifiers(_), TermName(xx), _,
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
     *   ((x: Int, y: Int) => x + y * 2)(3, 44)
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
