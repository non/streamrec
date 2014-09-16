package streamrec

import scala.{specialized => sp}

import language.experimental.macros
import scala.reflect.macros.Context
import scala.annotation.tailrec
import scala.collection.mutable

trait InfStream[@sp A] {
  def nth(n: Int): A
  def stream: scala.collection.immutable.Stream[A]
  def vector(n: Int): Vector[A]
}

object Macros {

  def infinite1[A, B](start: () => B, step: B => B, result: B => A): InfStream[A] =
    macro inf1[A, B]

  def infinite2[A, B, C](start: () => (B, C), step: (B, C) => (B, C), result: (B, C) => A): InfStream[A] =
    macro inf2[A, B, C]

  def infinite3[A, B, C, D](start: () => (B, C, D), step: (B, C, D) => (B, C, D), result: (B, C, D) => A): InfStream[A] =
    macro inf3[A, B, C, D]

  def inf1[A: ct.WeakTypeTag, B: ct.WeakTypeTag](ct: Context)
    (start: ct.Expr[() => B], step: ct.Expr[B => B], result: ct.Expr[B => A]): ct.Expr[InfStream[A]] = {

    import ct.universe._
    import definitions._

    val util = new Util[ct.type](ct)

    util.verifyAnonymousFunction(start.tree)
    util.verifyAnonymousFunction(step.tree)
    util.verifyAnonymousFunction(result.tree)

    val List(i, n, v, b, b2) =
      util.names("i", "n", "v", "b", "b2")

    val A = util.tpe[A]
    val B = util.tpe[B]

    val tree = q"""
      new streamrec.InfStream[$A] {
        def nth($n: Int): $A = {
          @scala.annotation.tailrec def loop($i: Int, $b: $B): $A =
            if ($i <= 1) {
              $result.apply($b)
            } else {
              val $b2 = $step.apply($b)
              loop($i - 1, $b2)
            }
          val $b = $start.apply()
          loop($n, $b)
        }

        def stream: scala.collection.immutable.Stream[$A] = {
          def next($b: $B): scala.collection.immutable.Stream[$A] =
            $result.apply($b) #:: {
              val $b2 = $step.apply($b)
              next($b2)
            }
          val $b = $start.apply()
          next($b)
        }

        def vector($n: Int): Vector[$A] = {
          val $v = new scala.collection.immutable.VectorBuilder[$A]

          @scala.annotation.tailrec def loop($i: Int, $b: $B): Unit =
            if ($i > 1) {
              $v += $result.apply($b)
              val $b2 = $step.apply($b)
              loop($i - 1, $b2)
            }

          val $b = $start.apply()
          loop($n, $b)
          $v.result
        }
      }
    """

    util.inlineCleanupAndReset[InfStream[A]](tree)
  }

  def inf2[A: ct.WeakTypeTag, B: ct.WeakTypeTag, C: ct.WeakTypeTag](ct: Context)
    (start: ct.Expr[() => (B, C)], step: ct.Expr[(B, C) => (B, C)], result: ct.Expr[(B, C) => A]): ct.Expr[InfStream[A]] = {

    import ct.universe._
    import definitions._

    val util = new Util[ct.type](ct)

    util.verifyAnonymousFunction(start.tree)
    util.verifyAnonymousFunction(step.tree)
    util.verifyAnonymousFunction(result.tree)

    val List(i, n, v, b, b2, c, c2) =
      util.names("i", "n", "v", "b", "b2", "c", "c2")

    val A = util.tpe[A]
    val B = util.tpe[B]
    val C = util.tpe[C]

    val tree = q"""
      new InfStream[$A] {
        def nth($n: Int): $A = {
          @scala.annotation.tailrec def loop($i: Int, $b: $B, $c: $C): $A =
            if ($i <= 1) {
              $result.apply($b, $c)
            } else {
              val ($b2, $c2) = $step.apply($b, $c)
              loop($i - 1, $b2, $c2)
            }
          val ($b, $c) = $start.apply()
          loop($n, $b, $c)
        }

        def stream: scala.collection.immutable.Stream[$A] = {
          def next($b: $B, $c: $C): scala.collection.immutable.Stream[$A] =
            $result.apply($b, $c) #:: {
              val ($b2, $c2) = $step.apply($b, $c)
              next($b2, $c2)
            }
          val ($b, $c) = $start.apply()
          next($b, $c)
        }

        def vector($n: Int): Vector[$A] = {
          val $v = new scala.collection.immutable.VectorBuilder[$A]
          sys.error("oh no!!!!")

          @scala.annotation.tailrec def loop($i: Int, $b: $B, $c: $C): Unit =
            if ($i > 1) {
              $v += $result.apply($b, $c)
              val ($b2, $c2) = $step.apply($b, $c)
              loop($i - 1, $b2, $c2)
            }

          val ($b, $c) = $start.apply()
          loop($n, $b, $c)
          $v.result
        }
      }
    """

    val e = util.inlineCleanupAndReset[InfStream[A]](tree)
    //println(showRaw(e))
    //println(show(e))
    e
  }

  def inf3[A: ct.WeakTypeTag, B: ct.WeakTypeTag, C: ct.WeakTypeTag, D: ct.WeakTypeTag](ct: Context)
    (start: ct.Expr[() => (B, C, D)], step: ct.Expr[(B, C, D) => (B, C, D)], result: ct.Expr[(B, C, D) => A]): ct.Expr[InfStream[A]] = {

    import ct.universe._
    import definitions._

    val util = new Util[ct.type](ct)

    util.verifyAnonymousFunction(start.tree)
    util.verifyAnonymousFunction(step.tree)
    util.verifyAnonymousFunction(result.tree)

    val List(i, n, v, b, b2, c, c2, d, d2) =
      util.names("i", "n", "v", "b", "b2", "c", "c2", "d", "d2")

    val A = util.tpe[A]
    val B = util.tpe[B]
    val C = util.tpe[C]
    val D = util.tpe[D]

    val tree = q"""
      new InfStream[$A] {
        def nth($n: Int): $A = {
          @scala.annotation.tailrec def loop($i: Int, $b: $B, $c: $C, $d: $D): $A =
            if ($i <= 1) {
              $result.apply($b, $c, $d)
            } else {
              val ($b2, $c2, $d2) = $step.apply($b, $c, $d)
              loop($i - 1, $b2, $c2, $d2)
            }
          val ($b, $c, $d) = $start.apply()
          loop($n, $b, $c, $d)
        }

        def stream: scala.collection.immutable.Stream[$A] = {
          def next($b: $B, $c: $C, $d: $D): scala.collection.immutable.Stream[$A] =
            $result.apply($b, $c, $d) #:: {
              val ($b2, $c2, $d2) = $step.apply($b, $c, $d)
              next($b2, $c2, $d2)
            }
          val ($b, $c, $d) = $start.apply()
          next($b, $c, $d)
        }

        def vector($n: Int): Vector[$A] = {
          val $v = new scala.collection.immutable.VectorBuilder[$A]

          @scala.annotation.tailrec def loop($i: Int, $b: $B, $c: $C, $d: $D): Unit =
            if ($i > 1) {
              $v += $result.apply($b, $c, $d)
              val ($b2, $c2, $d2) = $step.apply($b, $c, $d)
              loop($i - 1, $b2, $c2, $d2)
            }

          val ($b, $c, $d) = $start.apply()
          loop($n, $b, $c, $d)
          $v.result
        }
      }
    """

    util.inlineCleanupAndReset[InfStream[A]](tree)
  }

  /**
   * The Util class supports inlining and related optimizations.
   */
  class Util[C <: Context with Singleton](val c:C) {
    import c.universe._

    def tpe[A](implicit ev: c.WeakTypeTag[A]) = ev.tpe

    def tpes[A, B](implicit eva: c.WeakTypeTag[A], evb: c.WeakTypeTag[B]) =
      List(eva.tpe, evb.tpe)
    def tpes[A, B, C](implicit eva: c.WeakTypeTag[A], evb: c.WeakTypeTag[B], evc: c.WeakTypeTag[C]) =
      List(eva.tpe, evb.tpe, evc.tpe)
    def tpes[A, B, C, D](implicit eva: c.WeakTypeTag[A], evb: c.WeakTypeTag[B], evc: c.WeakTypeTag[C], evd: c.WeakTypeTag[D]) =
      List(eva.tpe, evb.tpe, evc.tpe, evd.tpe)

    def name(s: String) = newTermName(c.fresh(s + "$"))

    def names(ss: String*) = ss.toList.map(name)

    def isAnonymousFunction(t: Tree): Boolean = t match {
      case Function(_, _) => true
      case _ => false
    }

    def verifyAnonymousFunction(t: Tree) {
      if (!isAnonymousFunction(t))
        c.abort(c.enclosingPosition, "Arguments required to be anonymous functions")
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

      def handle(trees: List[Tree]): List[Tree] = {
        def rewrite(args: List[Tree], tn: String, rest: List[Tree]): List[Tree] =
          matchTplDerefs(args.length, tn, rest) match {
            case Some((names, rest)) =>
              names.zip(args).map { case (name, arg) =>
                ValDef(Modifiers(), TermName(name), TypeTree(), arg): Tree
              } ::: handle(rest)
            case None =>
              trees.head :: handle(rest)
          }

        // TODO: make this less brittle
        trees match {
          case ValDef(_: Modifiers, TermName(xx), _, Match(Annotated(Apply(Select(New(Select(Ident(_), _)), _), Nil), Apply(TypeApply(Select(Select(Ident(scala), TermName(tn)), ApplyName), _), args)), _)) :: rest if tn.startsWith("Tuple") =>
            rewrite(args, tn, rest)

          case t :: ts =>
            t :: handle(ts)

          case Nil =>
            Nil
        }
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

    def inlineCleanupAndReset[T](tree: Tree): c.Expr[T] = {
      val inlined = inlineApplyRecursive(tree)
      val fixed = Fixer.transform(inlined)
      val cleaned = Cleanup.transform(fixed)
      c.Expr[T](c.resetLocalAttrs(cleaned))
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
