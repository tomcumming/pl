### Datatypes

```
data Never {}
data Unit { Unit }
data Pair a b { Pair a b }
data Either a b { Left a; Right b }
data Bool { True; False }
```

### Pattern matching

```
not = \b -> match b {
    True -> False;
    False -> True;
};
```

### Linearity

```
unused_arg = \x -> \y -> x;

unused_arg_actual \x -> \y -> match drop y {
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

use_twice_actual = \(x: String) -> match implicitCopy x {
    Pair x y -> Pair x y
}
```

```
use_twice_fixed = \(x: String) -> copy x;
```

```
class Copy a {
    copy : a -> Pair a a;
}

class ImplicitCopy a <= Copy a {
    implicitCopy : a -> Pair a a;
}
```

### Functions

```
\x -> x : forall a. Fn () a a
\(x: U32) -> \(y: U32) -> add x y : Fn () U32 (Fn U32 U32 U32)

impl Copy (Fn a b c) <= Copy a { /* ... */ }
impl Drop (Fn a b c) <= Drop a { /* ... */ }

```

..._talk about signatures aka (->)_...

### Sharing

```
'Hello World!' : Static String
```

```
class Shared s { /* ... */ }

impl Shared Static { /* ... */ }
impl Shared Rc { /* ... */ }
```

### Bounded Values

```
bounded : a -> (forall 'x. Bounded 'x a -> b) -> Pair a b

impl Copy (Bounded 'x a) { /* ... */ }
impl Drop (Bounded 'x a) { /* ... */ }
```

```
get : Shared s => s (Vec a) -> USize -> Maybe (s a)
```
