---
title:  "Java Sequence"
date: 2016-10-16 22:00:00
permalink: /java-sequence/
---

In this article I would like to explore a `sequence` function and its implementation in Java.

```java
Applicative<Traversable<T>> sequence(Traversable<Applicative<T>> values)
```

You can think about `Traversable<T>` as an interface that describes a container of values `T`, something like `Iterable<T>`. I am going to use `T[]` and `List<T>` as an example.

`Applicative<T>` is sort of context for values `T`, and it allows to join several such values in contexts together. There is no similar interface in Java
but there are several types that have similar behavior.

So sequence is a function that for a given container of wrapped values produces a wrapped container of values.

Let me walk you through some common Java types with Applicative semantics and explain sequencing by example.  

#### Optional


```java
Optional<List<T>> sequence(Optional<T> ... optionals)
```

`Optional` represents a value that can be absent. Joining together several potentially absent values produces a potentially absent result.

So a sequence of optional values is going to produce an optional list, and it will be present as long as all of optionals are present.

Consider a function `parse` that extracts a number from a string, if it represents a valid integer.

```java
Optional<Integer> parse(String number)
```

We can use sequence to group individual parsed numbers into parsed list.  

```java
sequence(parse("1"), parse("2"), parse("3"))
// Optional[[1,2,3]]

sequence(parse("1"), parse("X"), parse("3"))
// Optional.empty
```

Such semantics is useful when we cannot just ignore empty values and need to invalidate the whole list as soon as one of items is empty.

#### CompletableFuture

```java
CompletableFuture<List<T>> sequence(CompletableFuture<T> ... futures)
```

`CompletableFuture` is a representation of asynchronous computation that will provide a value in the future.

If we sequence a list of futures it should produce an asynchronous list that is going to be available later, after completion of all futures.

It also has all or nothing semantics - result will be available only after all of futures are completed.

```java
CompletableFuture<T> async(T i) // produces later
CompletableFuture<T> failed()   // finishes exceptionally
```

```java
sequence(async(1), async(2), async(3))
// CompletableFuture[[1,2,3]]

sequence(async(1), failed(), async(3))
// CompletableFuture.failed
```

It allows to build continuations without blocking on individual results

```java
sequence(async(1), async(2), async(3)).thenApply(this::sum)
// CompletableFuture[[6]]
```

#### List

```java
List<List<T>> sequence(List<T> ... lists)
```
Previously I've used `List` as an example for container. But it can also be treated as an Applicative.

List represents a choice between zero to many possible values. Joining several choices together leads to multiplication of possibilities.

In this sense sequence of lists produces their Cartesian product.

```java
sequence(asList(1, 2), asList(10, 20), asList(100))
// [[1, 10, 100], [1, 20, 100], [2, 10, 100], [2, 20, 100]]

sequence(asList(1, 2), emptyList(), asList(10, 20))
// []
```

Usually Cartesian product is used to generate combinations of values

```java
sequence(asList("J", "Q", "K", "A"), 
         asList("Clubs", "Diamonds", "Hearts", "Spades"))
// [[J, Clubs], [J, Diamonds], [J, Hearts], [J, Spades],
// [Q, Clubs], [Q, Diamonds], [Q, Hearts], [Q, Spades],
// [K, Clubs], [K, Diamonds], [K, Hearts], [K, Spades],
// [A, Clubs], [A, Diamonds], [A, Hearts], [A, Spades]]
```

#### Function

```java
Function<A, List<T>> sequence(Function<A, T> ... functions)
BiFunction<A, B, List<T>> sequence(BiFunction<A, B, T> ... functions)
```

`Function` can be also viewed as a context for value, a value that will be computed from some key.

Sequence of functions is a function that for a given input computes a list of values.

```java
sequence(Person::name, Person::surname).apply(person("John", "Doe"))  
// [John, Doe]

sequence(Integer::sum, Integer::max, Integer::min).apply(100, 200)
// [300, 200, 100]
```

## Implementation

The most common approach to implement `sequence` is to fold over items. Java's analogy would be a reduction of `Stream`

```java
<T> Optional<List<T>> sequence(List<Optional<T>> optionals) {
  return optionals.stream().reduce(
// Initial value Optional<List<T>>
    Optional.of(new ArrayList<>()),

// Accumulator BiFunction<Optional<List<T>>, Optional<T>, Optional<List<T>>>
    (result, optional) -> result.flatMap(list -> optional.map(item -> {
      list.add(item);
      return list;
    })),

// Combiner BinaryOperator<Optional<List<T>>  
    (result, chunk) -> result.flatMap(left -> chunk.map(right -> {
      List<T> r = new ArrayList<>(left);
      r.addAll(right);
      return r;
    })));
}
```

`Identity` value in reduction is an option of empty list (which is going to be a result if stream is empty). `Accumulator` joins together previously accumulated optional list and current optional value. Finally `combiner` takes two optional lists that have been produced in parallel and joins them together.

Note that both accumulator and combiner will produce an empty `Optional` if at least one of arguments is empty.

Accumulator and combiner has the same structure - if both optionals are present then function is applied to their arguments. Lets exploit this pattern and make some refactoring

```java
<T> Optional<List<T>> sequence(List<Optional<T>> optionals) {
  return optionals.stream().reduce(
    pure(emptyList()),
    lift(add()),
    lift(addAll()));
}

<A, B, C> BiFunction<Optional<A>, Optional<B>, Optional<C>>
lift(BiFunction<A, B, C> f) {
  return (oa, ob) -> oa.flatMap(a -> ob.map(b -> f.apply(a, b)));
}

<A> BinaryOperator<Optional<A>> lift(BinaryOperator<A> f) {
  return (oa, ob) -> oa.flatMap(a -> ob.map(b -> f.apply(a, b)));
}

<T> Optional<T> pure(T value) {
  return Optional.of(value);
}

<T> BiFunction<List<T>, T, List<T>> add() {
  return (ts, t) -> {
    ArrayList<T> result = new ArrayList<>(ts);
    result.add(t);
    return result;
  };
}

<T> BinaryOperator<List<T>> addAll() {
  return (ts, ts2) -> {
    ArrayList<T> result = new ArrayList<>(ts);
    result.addAll(ts2);
    return result;
  };
}
```
Note 2 functions:

 * `lift` transforms a function to work on `Optional` arguments
 * `pure` wraps a value into `Optional`

These functions are part of `Applicative`, an interface which satisfy all previous example types. It means that for all of them reduction will look the same, as soon as we manage to provide `lift` and `pure` implementations.

Let's do it for `CompletableFuture`

```java
<T> CompletableFuture<List<T>> 
sequence(List<CompletableFuture<T>> futures) {
  return futures.stream().reduce(
    pure(emptyList()),
    lift(add()),
    lift(addAll()));
}

<T> CompletableFuture<T> pure(T t) {
  return completedFuture(t);
}

<A, B, C> BiFunction<CompletableFuture<A>, CompletableFuture<B>, CompletableFuture<C>>
lift(BiFunction<A, B, C> f) {
  return (fa, fb) -> fa.thenCombine(fb, f);
}

<A> BinaryOperator<CompletableFuture<A>> lift(BinaryOperator<A> f) {
  return (fa, fb) -> fa.thenCombine(fb, f);
}
```

Bodies of `sequence` implementations are identical, that is a place for generalization. Unfortunately Java's type system is not powerful enough to represent generic type with generic parameter, i.e. [Generics of higher kind](http://adriaanm.github.io/files/higher.pdf).

So we cannot extract type safe notion of `Applicative` for `sequence` function

```java
<T, A extends Applicative> A<List<T>> sequence(A<T> ... applicatives)
```

## Composition

Anyway we have a space for reuse. In my daily work Streams become a tool for composition of operations over some collection of items. A situation when I need to implement similar reduction are not unique.

I have two options:

 * collect result to list and use sequence function over it
 * reimplement the same reduction as above

Collections of list to stream usually is a little bit premature. So we need to extract reduction. The way to reuse reduction functionality in `Stream` API is to create a `Collector`.

```java
<T> Collector<Optional<T>, ?, Optional<List<T>>> optionals()

Optional<List<Integer>> result =
  Stream.of(parse("1"), parse("2"), parse("3")).collect(optionals())
//Optional[1, 2, 3]
```

We can go one step further and generalize resulting container, by using composition of collectors.

```java
<T, A, R> Collector<Optional<T>, ?, Optional<R>>
optionals(Collector<T, A, R> downstream) {
  return collector(
    lift(downstream.supplier()),
    lift(accumulator(downstream)),
    lift(combiner(downstream)),
    lift(downstream.finisher()));
}
```

It takes a collector of values and lifts it into `Optional` context such that it collects optionals with semantics of `sequence` operation.

Its implementation is a little bit more involved but has the same approach - it lifts each part of downstream collector into `Optional` context and constructs a new collector.

The full implementation of collectors for all previous types is in [this repo](http://github.com/nbardiuk/sequencedemo)

Now we can use it as a last step of stream processing

```java
Stream.of(parse("1"), parse("2")).collect(optionals(toList()))
// Optional[[1, 2]]

Stream.of(parse("1"), parse("X")).collect(optionals(toList()))
// Optional.empty
```

Composition of collectors gives us useful flexibility - we can reuse existing collectors from JDK and external libraries.

```java
Stream.of(async("1"), async("2"), async("3")).collect(futures(joining(":")))
// CompletableFuture["1:2:3"]

Optional<Map<Boolean, List<Integer>>> result =
Stream.of(parse("13"), parse("12"), parse("11"))
  .collect(optionals(groupingBy(i -> i % 2 == 0)))
// Optional[{false=[11, 13], true=[12]}]
```

Also we can compose sequencing collectors. Consider a list of futures that will complete with optional result.

```java
List<CompletableFuture<Optional<Integer>>>
list = asList(async(parse("1")), async(parse("2")), async(parse("3")))
```
Using composition of collectors we can decide how much structure should be extracted from list

```java
CompletableFuture<List<Optional<Integer>>>
result = list.stream().collect(futures(toList()))
// CompletableFuture[[Optional[1], Optional[2], Optional[3]]]

CompletableFuture<Optional<List<Integer>>>
result = list.stream().collect(futures(optionals(toList())))
// CompletableFuture[Optional[[1, 2, 3]]]
```

We are not limited here by number of layers that can be composed.

Lets go crazy with functions

```java
List<Function<String, CompletableFuture<Optional<Integer>>>>
list = asList(s -> async(parse(s)), s -> async(read(s)))

Function<String, List<CompletableFuture<Optional<Integer>>>>
result = list.stream().collect(functions(toList()))

Function<String, CompletableFuture<List<Optional<Integer>>>>
result = list.stream().collect(functions(futures(toList())))

Function<String, CompletableFuture<Optional<List<Integer>>>>
result = list.stream().collect(functions(futures(optionals(toList()))))
```
each composed sequence pushes `List` deeper and deeper inside a stack of contexts.

## Conclusion

I hope that today you've learned about `Applicative` and an operation that it enables - `sequence`.

Also we have learned that Java type system is not the most powerful but definitely have an API that enables composition.

You can checkout code examples from [this repo](http://github.com/nbardiuk/sequencedemo)

`Have a nice hack ;)`
