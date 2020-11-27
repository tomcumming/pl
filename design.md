## Datatypes

```
data Never {}
data Unit { Unit }
data Pair a b { Pair a b }
data Either a b { Left a; Right b }
data Bool { True; False }
```

```
Pair a b    =   (a, b)
```

## Pattern matching

```
not = \b -> match b {
    True -> False;
    False -> True;
};
```

## Linearity

```
unused_arg = \x -> \y -> x;
```
Gets rewritten as:
```
unused_arg = \x -> \y -> match drop y {
    Unit -> x;
};
```

```
class Drop a {
    drop : a -> Unit;
}
```

```
use_twice = \(x: String) -> (x, x)
```
Gets rewritten as:
```
use_twice = \(x: String) -> match implicitCopy x {
    (x, y) -> (x, y)
}
```

```
use_twice_works = \(x: String) -> copy x;
```

```
class Copy a {
    copy : a -> (a, a);
}

class ImplicitCopy a <= Copy a {
    implicitCopy : a -> (a, a);
}
```

## Functions

Functions have to carry their closure in the type signature! `Fn a b c` describes a function that takes a type `b` and returns a `c` while having a closure of type `a`.

```
\x -> x : forall a. Fn () a a
\(x: Word) -> \(y: Word) -> add x y : Fn () Word (Fn Word Word Word)

impl Copy (Fn a b c) <= Copy a { /* ... */ }
impl Drop (Fn a b c) <= Drop a { /* ... */ }

```

..._talk about classic signatures aka (a -> b -> c -> d)_...
```
a -> b -> c -> d    =   Fn () a (Fn a b (Fn (a, b) c d))
```

## Sharing

```
class Shared (s : * -> *) { /* ... */ }

impl Shared RC { /* ... */ }
impl Shared ARC { /* ... */ }
```

## Bounded Values

```
'Hello World!' : Bound Static String
```

Use the `ST` monad trick to create regions:

```
bound : a -> (forall 'r. (Cast Static 'r, Bound 'r a) -> b) -> (a, b)

impl Shared (Bound 'r)
impl Copy (Bound 'r a) { /* ... */ }
impl Drop (Bound 'r a) { /* ... */ }
```

```
get : Shared s => s (Vec a) -> USize -> Maybe (s a)
```

```
subBound
    : Cast _ 'r
    -> a
    -> (forall 's. (Cast 'r 's, Bound 's a) -> b)
    -> (a, b)
```

## Casting

```
cast : a -> Cast a b -> b

castVec : Cast a b -> Cast (Vec a) (Vec b)
castBound : Cast x y -> Cast (Bound 'a x) (Bound 'a y)
limitBound : Cast 'r 's -> Cast (Bound 'r a) (Bound 's y)
```

_TODO: example casting something awkward_

Might be able to automate this with type classes and higher rank constraints ?

## IO

`World` does not implement `Drop` nor `Copy`, if it is always evaluated in place then it should be safe to do things like print a `Bound 'a String`.

```
World : *

printString : w -> String -> w
printSharedString : Shared s => w -> s String -> w

readLine : w -> (w, String)
```
Maybe we can still have something like the `IO` monad if `IO a` does not implement drop nor copy?
```
type IO a = {
    IO(Fn () w (w, a)) -- needs to be private?
}

bind : IO a -> Fn c a (IO b) -> IO b
```

## Monads

```
bindIO : IO a -> Fn c a (IO b) -> IO b
bindVec : Vec a -> Fn c a (Vec b) -> Vec b
bindMaybe : Drop c => Maybe a -> Fn c a (Maybe b) -> Maybe b
bindEither : Drop c => Either a b -> Fn d b (Either a c) -> Either a c
```

How will these varying constraints affect things...?
