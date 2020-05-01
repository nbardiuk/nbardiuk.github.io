---
title: "What I've learned during Advent of Code"
date:  2018-11-08 00:00:00
---

> Advent of Code is a series of small programming puzzles for a variety of skill sets and skill levels in any programming language you like.
> People use them as a speed contest, interview prep, company training, university coursework, practice problems, or to challenge each other.
>
> [About Advent of Code][adventabout]


Puzzles are fun and Advent of Code has 25 of them!
Tasks are connected by common narrative and are published every day in December until Christmas.
Schedule keeps me motivated to complete each challenge withing days it was published so I can avoid spoilers.
Also on [reddit][reddit] people discuss their solutions in terms of lines of code, performance or interesting insights they discovered.


Last year I've challenged myself to use only TDD and FP in Scala for [my solutions][src].
I would like to share what I've learned from the challenge and how I am going to challenge myself this time.


## TDD

I restricted myself to pure TDD where I would write minimal test first and then only implement enough to fix failing test.

### Inside Out

My first approach was to decompose puzzle into small problems and start from writing a test for one of them.
It is relaxing coding experience - no need to think. 
All hard work was done during design of the test case, all I need to do is just to transform input into correct output.

For complicated problems if I made a mistake during decomposition I had to not only fix the code but also I had to rewrite the whole test specification. My tests where using private API of algorithm implementation and if it is wrong everything goes to the bin.

Some challenges where hard for me to implement using only immutable data structures.
So I would cheat with mutable internal state while trying to keep public signature pure.
Mutable state requires more complicated test setup comparing to just passing parameters into a function and checking output.

### Outside In

Next approach was to write test for problem using its public interface.
I could use example scenario from problem definition as my first test case or maybe try to think for something simpler.

For me it is more satisfying way to code.
It creates a safety net on the perimeter and allows do be more creative inside.

In this case tests can survive most of internal changes.
The biggest downside I discovered was that some tests had very wide scope. Coding mistakes are harder to debug, it takes longer to find where it went wrong.

### Goof around

I ended up using mixed approach.
If problem wasn't that hard I would just create a test case from example provided in the problem definition.
Or I could start with tests for decomposed problem but as soon as I reach public API I could delete smaller tests.

My important takeaway was that it is OK to remove some intermediate tests once implementation reaches some end to end functionality.


### Refactoring

Daily challenge consists of 2 parts, where second part would build on top of first one. 
Usually first part can be solved using simple suboptimal algorithm but second one requires to optimize it in some way (that I never could guess).
It was great opportunity to practice Kent Beck advice on refactoring 

> When you find you have to add a feature to a program, and the program's code is not structured in a convenient way to add the feature, first refactor the program to make it easy to add the feature, then add the feature.

And then I had my own third part for the day - looking others solutions on [reddit][reddit].
Sometimes I would come back with new ideas and good tests should keep me safe without spoiling fun.


## FP in Scala

I was interested how far I can get with pure functions to solve algorithmic problems.

Simple problems are so straightforward in FP that I feel almost like cheating.
I can restate problem definition by composing pure functions and it produces the right answer.
```scala
def countUnique(passphrases: Seq[String]): Int = passphrases.count(unique)
def unique(passphrase: String): Boolean = unique(words(passphrase))
def words(passphrase: String) = passphrase.split(' ')
def unique[T](ts: Seq[T]): Boolean = ts.toSet.size == ts.size 
```

Complex problems require to use another way of thinking.
Lazy Streams and recursion were great tools to keep code pure and readable.
But for some cases I had to fall back to mutable Arrays and Maps.

Avoidance of mutability stimulated me to explore functional programming beyond what I thought I want.
In one such exploration I've learned about [Fixed-point combinator][fixedpoint] and how to [memoize][memo] recursive functions.
```scala
def fibonachi: Int => Int =
  fixMemo[Int, Int] { fibonachi => n =>
    if (n == 1 || n == 0) 1
    else fibonachi(n-1) + fibonachi(n-2)
  }(_)

private def fixMemo[I, O](fix: (I => O) => I => O): I => O = {
  lazy val io: I => O = memo()(fix(io)(_))
  io
}

private def memo[I, O](cache: mutable.Map[I, O] = mutable.HashMap[I, O]())(
  iToO: I => O): I => O =
  i => cache.getOrElseUpdate(i, iToO(i)
```

## This year

This time I am going to use Haskell.
I want to make sure I don't slip into mutable arrays and maps again.
Also I am interested to see how lazy programming language will affect my approach to solutions.

My main goal this year is to actually complete all 25 puzzles.
And I expect to learn couple new tricks from fellow puzzle solvers.



[adventabout]: https://adventofcode.com/about
[src]: https://github.com/nbardiuk/adventofcode2017
[reddit]: https://www.reddit.com/r/adventofcode
[fixedpoint]: https://en.wikipedia.org/wiki/Fixed-point_combinator
[memo]: https://michid.wordpress.com/2009/02/23/function_mem/
