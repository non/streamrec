Proof-of-concept for generating instances by rewriting anonymous functions.

## Overview

Given this trait:

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}
```

We use Scala macros to turn this:

```scala
val fibs: InfStream[Int] =
  Macros.infinite2[Int, Int, Int]((0, 1), (b, c) => (c, b + c), (b, c) => b)
```

Into this:

```scala
val fibs: InfStream[Int] =
  new InfStream[Int] {
    def nth(n: Int): Int = {
      @tailrec def loop(i: Int, b: Int, c: Int): Int =
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
  
    def stream: Stream[Int] = {
      def next(b: Int, c: Int): Stream[Int] =
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

## Motivation

Say we have a trait representing an infinite stream of numbers (Fibonacci
sequence, digits of Pi, whatever). We can represent that as a `Stream[A]` but
if we're only interested in a single result the stream imposes a lot of
overhead. In those cases we'd prefer to use a simple tail-recursive method.
However, in other cases we might want all the values, which the stream will do
a better job at. We might even dream up other related methods: allocating the
first N values into an Array, using a different stream implementation like
`StreamT`, and so on.

What we want is a single trait that links these implementations, since the
underlying machinery (the rules for generating the next sequence value) will
be completely shared between all the methods in question.

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}
```

How to make this work?

An easy way to go is to use tuples (or some other intermediate object) to
store the intermediate state for our sequence. We can define our starting
state, a function to get the next state tuple from the current state, and a
function to turn a state tuple into a sequence value. For the fibonacci
sequence, we'd use something like:

```scala
val start = (0L, 1L)
def step(x: Long, y: Long) = (y, x + y)
def result(x: Long, y: Long) = x
```

However, we'll still be building (and tearing apart) a tuple at every
step of our sequence. To generate the nth Fibonacci number, we'd
probably prefer something like:

```scala
def nth(n: Int): Long = {
  @tailrec def fib(i: Long, x: Long, y: Long): Long =
    if (i > 0) fib(i - 1, y, x + y) else x
  fib(n, 0L, 1L)
}
```

It's easy to see how given `start`, `step`, and `result` we can easily build
an implementation of `nth`. The same is true for stream generation, and the
code looks very similar:

```scala
def stream: Stream[Long] = {
  def next(x: Long, y: Long): Stream[Long] =
    x #:: next(y, x + y)
  next(0L, 1L)
}
```

Using macros, we can generate all this code from the user-provided anonymous
functions and starting values.

Each anonymous function takes N parameters and returns a tuple with N fields.
In our `nth` method we build a tail-recursive method that doesn't bother
allocating the tuple, but plugs the values directly into the next call. In the
`stream` case we do something similar, but instead of recursion we're
allocating by-name parameters that defer the next call until the stream needs
it. The whole thing is a work-around to the lack of multiple return values on
the JVM.

## Future Work

I'd like to optimize the segmented sieve implementation until I can generate
the 1-billionth prime in a "reasonable" amount of time. The whole idea for
this project came out of wanting better support for infinite series and
sequences in Spire.

It would also be nice to include a non-memoizing stream implementation, since
for very large streams it's easy to accidentally memoize the entire thing.

## Caveats

The basic machinery is working but not yet well-tested.

Probably the least attractive thing right now is that we need separate
macros for `infinite1`, `infinite2`, `infinite3`, and so on. What's
more, since we're currently using `reify` we have a huge amount of
boiler-plate shard between each of these, differing only by the number
of parameters.

There is currently a small risk of name collisions, since it's not possible to
use `reify` while generating fresh names. Unless you like to begin your names
with double-underscores (e.g. `__n`) it won't be a problem.
