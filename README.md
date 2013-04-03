Proof-of-concept for generating instances by rewriting anonymous functions.

## Purpose

We want to turn something like this:

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}

val fibs: InfStream[Int] =
  Macros.infinite2[Int, Int, Int]((0, 1), (b, c) => (c, b + c), (b, c) => b)
```

Into an instance like this:

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}

val fibs: InfStream[Int] =
  new InfStream[A] {
    def nth(n: Int): A = {
      @tailrec def loop(i: Int, b: B, c: C): A =
        if (i < 1) {
          b
        } else {
          val b2 = c
          val c2 = b + c
          loop(i - 1, b2, c2)
        }
      val b = 0
      val c = 1
      loop(n, b, c)
    }
  
    def stream: Stream[A] = {
      def next(b: B, c: C): Stream[A] =
        b #:: {
          val b2 = c
          val c2 = b + c
          next(b2, c2)
        }
      val b = 0
      val c = 1
      next(b, c)
    }
  }
```

## Description

Each anonymous function takes N parameters and returns a tuple with N fields.
In our `nth` case what we want to do is build a tail-recursive method that
doesn't bother allocating the tuple, but plugs the value directly into the
next call. In the `stream` case we do something similar. The whole thing is a
work-around to the lack of multiple return values on the JVM.

## Caveats

The basic machinery is working (sort of), but it's not finished.
Known issues:

* Not verifying that function arguments are really anonymous functions.
* Not rewriting function parameters to match the names we're using in `reify`.
* Only handles anonymous functions with a single exit point.

None of these issues should be that hard to fix.

The project is using macro-paradise branch of Scala 2.11 just because I
initially planned to use type macros. However, the macros currently available
in 2.10 are actually up to the task, so I may revert to 2.10.0.
