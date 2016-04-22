---
layout: post
title:  "Monads with Java 8"
date:   2015-11-08 22:00:00
categories: Java Monad Haskell
permalink: /java-monad
---
It is a paraphrase to Java of ["The Trivial Monad"][trivial-monad] by Dan Piponi with applications for Java developers.

### An intuition

Monad is like a ...

There is no good analogy to explain what is monad to new person ([monad tutorial fallacy][monad-fallacy]). The best way to get it right is just to do some coding. So lets explore simple design pattern - one-way wrapper. Such wrapper can wrap value but can't unwrap it back.

In Java it can look like simple immutable object

```java
@ToString @EqualsAndHashCode
class Wrap<T> {
  private final T value;
  private Wrap(T value) { this.value = value; }
  public static <T> Wrap<T> of(T value) {
    return new Wrap<>(value);
  }
}
```
This class is just going to wrap a value using factory method `of`. There are no getters and setters, but present `toString`, `equals` and `hashCode` for convenience (see [lombok]).

To do something with value we need a method that will apply mapping function and then return new wrapped value. It is important to keep value wrapped so it never escapes.

```java
class Wrap<T> {
  // ...
  public <R> Wrap<R> map(Function<T, R> mapper) {
    return Wrap.of(mapper.apply(value));
  }
}
```

```java
Wrap<Integer> a = Wrap.of(1);           // Wrap(value=1)
Wrap<Integer> b = a.map(i -> i + 9);    // Wrap(value=10)
Wrap<Integer> c = b.map(i -> i * 11);   // Wrap(value=110)
a.map(i -> i * 10).map(i -> i + 11);    // Wrap(value=21)
```
OK, now we can do something with this, but usually things are more interesting. After starting using wrapped values here and there we eventually create method like this

```java
Wrap<Integer> inc(Integer x) {
  return Wrap.of(x + 1);
}
```
`inc` gets a number and then returns a wrapped result. It is very useful business logic and we want to apply it even to wrapped values.

```java
Wrap<Integer> a = Wrap.of(1);     // Wrap(value=1)
a.map(this::inc);                 // Wrap(value=Wrap(value=2))
a.map(this::inc).map(this::inc);  // !!! COMPILATION ERROR
```
First problem we face is that it wraps already wrapped result. And then we cannot continue applying it - `inc` accepts only `Integer` but not `Wrap<Integer>` instance.

There should be some way to `inc` value and not wrap it again.

```java
class Wrap<T> {
  // ...
  public <R> Wrap<R> flatMap(Function<T, Wrap<R>> mapper) {
    return mapper.apply(value);
  }
}
```
`flatMap` is still safe - it doesn't give user a plain value but provides it to the mapper.

Now we can apply `inc` several times in chains

```java
Wrap<Integer> a = Wrap.of(1);              // Wrap(value=1)
a.flatMap(this::inc);                      // Wrap(value=2)
a.flatMap(this::inc).flatMap(this::inc);   // Wrap(value=3)
```
Actually `flatMap` is more generic then `map` and we can implement `map` using it

```java
class Wrap<T> {
  // ...
  <R> Wrap<R> map(Function<T, R> mapper) {
    return flatMap(mapper.andThen(Wrap::of));
  }
}
```
`andThen` composes function with another, passing result of first as argument to the second [Function#andThen]

Now we have all tools to work with our wrapper type. `of` wraps a value and `flatMap` gives a way to modify it without need to unwrap anything. And we can chain multiple transformations without worry how to unwrap layers of results.

Basically, this is a monad - type that provides APIs to enclose some value and modify it without exiting enclosed context

```java
interface Monad<T> {
  Monad<T> of(T value);
  <R> Monad<R> flatMap(Function<T, Monad<R>> mapper);
}
```

### Real world problems

In Java there are several monadic types and even more with growing number of libraries. I will use [Optional] next, but these examples can be similarly applied to others.

#### Operations on Optionals

Assume we need to add two optional values. And we don't know how to unwrap them, I mean don't know what to do with empty values. All what we want is only add values together and leave that decision for later

```java
Optional<Integer> a = ...
Optional<Integer> b = ...
return add(a, b);
```
Such function `add` should return new Optional with the result of adding values

```java
Optional<Integer> add(Optional<Integer> oa, Optional<Integer> ob) {
  return oa.flatMap(a -> ob.map(b -> a + b));
}
```
Lambda inside `flatMap` has access to value of `oa` and uses it to increment value of `ob` similarly to previous `inc` function.

```java
Optional<Integer> a = Optional.of(13);
Optional<Integer> b = Optional.of(42);
add(a, b);                 // Optional[55]
add(a, Optional.empty());  // Optional.empty
add(Optional.empty(), b);  // Optional.empty
```

What if we need to perform other operations? Lets create another method that additionally accepts an operation

```java
<A, B, R> Optional<R> compute(BiFunction<A, B, R> operation, Optional<A> oa, Optional<B> ob) {
  return oa.flatMap(a -> ob.map(b -> operation.apply(a, b)));
}
```
It is little bit to verbose but basically `compute` applies `operation` on values from optionals and returns optional result

```java
Optional<Integer> a = Optional.of(13);
Optional<Integer> b = Optional.of(42);
BiFunction<Integer, Integer, Integer> plus = (x, y) -> x + y;
BiFunction<Integer, Integer, Integer> times = (x, y) -> x * y;
compute(plus, a, b);    // Optional[55]
compute(times, a, b);   // Optional[546]
```

#### Streams of Optionals
So far so good. But with Java 8 we usually deal with a lot of streams. It is a common case when during pipeline we end up with stream of optional values. Now as we know how to perform operations on optionals lets find a product of all optional values in stream

```java
Optional<Integer> one = Optional.of(1);
Stream<Optional<Integer>> stream = Stream.of(1, 2, 3, 4).map(Optional::of);
stream.reduce(one, (acc, elem) -> compute(times, acc, elem));  // Optional[24]
stream = Stream.of(Optional.of(10), Optional.empty());
stream.reduce(one, (acc, elem) -> compute(times, acc, elem));  // Optional.empty
```
We provide initial value `one` and then compute product of accumulator and each value in stream.

APIs are not always so friendly. Lets look at the reduce but without initial value

```java
stream.reduce((acc, elem) -> compute(times, acc, elem));  // Optional[Optional[24]]
```
It wraps result into optional, because stream can be empty and we didn't provide any initial value.

#### Flattening
How can we deal with optional of optional without unwrapping it?
We can flatten it with `flatMap`

```java
Optional<Optional<Integer>> ooa = Optional.of(Optional.of(24));
Optional<Integer> oa = ooa.flatMap(o -> o); // Optional[24]
```
Function `o -> o` is called identity and actually it is so useful that you can find it in standard library [Function#identity][identity]

### Wrap-up
Sometimes it is better to operate on monads and leave decision how to unwrap them for later. Consider this next time when you will try to get value from Optional, CompletableFuture or some other monadic type. I hope you've learned here one or two methods how to simplify your design using operations on Monads.

Code is available on [Gist] - feel free to play with it.

`Have a nice hack ;)`

[trivial-monad]: http://blog.sigfpe.com/2007/04/trivial-monad.html
[monad-fallacy]: https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/
[identity]: http://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html#identity--
[Function#andThen]: http://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html#andThen-java.util.function.Function-
[lombok]: https://projectlombok.org/
[Gist]: https://gist.github.com/nbardiuk/91793d997bed62f36175
[Optional]: http://docs.oracle.com/javase/8/docs/api/java/util/Optional.html
