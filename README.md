Proof-of-concept for generating instances by rewriting anonymous functions.

## Usage

Given this trait:

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}
```

We turn this:

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

Say we have a trait representing an infinite stream of numbers
(Fibonacci sequence, digits of Pi, whatever). We can represent that as
a `Stream[A]` but if we're only interested in a single result the
stream imposes a lot of overhead. In those cases we'd prefer to use a
simple tail-recursive method. However, if we want many values the
stream gives us this naturally.

What we want is a single trait that links these two implementations,
since the underlying machinery (the rules for generating the next
sequence value) will be completely shared between the two.

```scala
trait InfStream[A] {
  def nth(n: Int): A
  def stream: Stream[A]
}
```

How to make this work?

An easy way to go is to use tuples to store the intermediate state for
our sequence. We can define our starting state, a function to get the
next state tuple from the current state, and a function to turn a
state tuple into a sequence value. For the fibonacci sequence, we'd
use something like:

```scala
val start = (0, 1)
def step(x: Int, y: Int) = (y, x + y)
def result(x: Int, y: Int) = x
```

However, we'll still be building (and tearing apart) a tuple at every
step of our sequence. To generate the nth Fibonacci number, we'd
probably prefer:

```scala
def nth(n: Int): Int = {
  def fib(i: Int, x: Int, y: Int): Int =
    if (i > 0) fib(i - 1, y, x + y) else x
  fib(n, 0, 1)
}
```

It's easy to see how given `start`, `step`, and `result` we could
generate an implementation of `nth`. We can do the same for stream
generation:

```scala
def stream: Stream[Int] = {
  def next(x: Int, y: Int): Stream[Int] =
    x #:: next(y, x + y)
  next(0, 1)
}
```

It would be great if we could generate all this code from this simple
functions!

## Description

Each anonymous function takes N parameters and returns a tuple with N
fields.  In our `nth` methodf we build a tail-recursive method that
doesn't bother allocating the tuple, but plugs the values directly
into the next call. In the `stream` case we do something similar, but
instead of recursion we're allocating by-name parameters that defer
the next call until the stream needs it. The whole thing is a
work-around to the lack of multiple return values on the JVM.

## Caveats

The basic machinery is working but not yet well-tested.

Probably the least attractive thing right now is that we need separate
macros for `infinite1`, `infinite2`, `infinite3`, and so on. What's
more, since we're currently using `reify` we have a huge amount of
boiler-plate shard between each of these, differing only by the number
of parameters.

I still don't fully understand quasi-quoting but that may be able to
solve the problems. I was tempted to build the trees "by hand" but
these instances are large enough that the resulting code would
probably be completely unreadable.
