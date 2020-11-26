## Datatypes

```
data Never {}
data Unit { Unit }
data Pair a b { Pair a b }
data Either a b { Left a; Right b }
data Bool { True; False }
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
use_twice = \(x: String) -> Pair x x;
```
Gets rewritten as:
```
use_twice = \(x: String) -> match implicitCopy x {
    Pair x y -> Pair x y
}
```

```
use_twice_works = \(x: String) -> copy x;
```

```
class Copy a {
    copy : a -> Pair a a;
}

class ImplicitCopy a <= Copy a {
    implicitCopy : a -> Pair a a;
}
```

## Functions

Functions have to carry their closure in the type signature!

```
\x -> x : forall a. Fn () a a
\(x: U32) -> \(y: U32) -> add x y : Fn () U32 (Fn U32 U32 U32)

impl Copy (Fn a b c) <= Copy a { /* ... */ }
impl Drop (Fn a b c) <= Drop a { /* ... */ }

```

..._talk about classic signatures aka (->)_...

## Sharing

```
'Hello World!' : Static String
```

```
class Shared (s : * -> *) { /* ... */ }

impl Shared Static { /* ... */ }
impl Shared Rc { /* ... */ }
```

## Bounded Values

Use the `ST` monad trick to create regions:

```
bound : a -> (forall 'x. Bound 'x a -> b) -> Pair a b

impl Shared (Bound 'x)
impl Copy (Bound 'x a) { /* ... */ }
impl Drop (Bound 'x a) { /* ... */ }
```

```
get : Shared s => s (Vec a) -> USize -> Maybe (s a)
```

## Casting

```
cast : a -> Cast a b -> b

castVec : Cast a b -> Cast (Vec a) (Vec b)
castBound : Cast x y -> Cast (Bound 'a x) (Bound 'a y)

Region : * -> *
region : (forall 'r. Region 'r -> a) -> a

reBound : Region 'r -> Cast (Bound 'a x) (Bound 'r x)
```
So two compare two `Vec`s where the items have different bounds:
```
myVec : Vec (Bound 'a U32)
otherVec : Vec (Bound 'b U32)

region (\r -> cast myVec (castVec (reBound r)) == cast otherVec (castVec (reBound r)))
```

Might be able to do this with type classes and higher rank constraints ?
