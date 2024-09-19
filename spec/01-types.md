# Types

## Given type constructors
These are the only given type constructors, along with some compelling reasoning
that they are given.

- `&` Intersection: At least one binary function must be given to relate types.
- `¬` (U+00ac) Complement: Should be a given since it defines what `unknown` is and includes.
- `~` \<unnamed>: Should be a given since it defines how the types interact. `~` is unary and will evaluate to `unknown` if its argument is `never`, and otherwise will evaluate to `never`.

## Union, difference and intersection
1. `unknown` and `never`
```
unknown & a == a
unknown + a == unknown

never & a == never
never + a == a
```

2. Functions
```
(a -> c) + (b -> d) == (a & b) -> c + d
(a -> c) & (b -> d) == (a + b) -> c & d
```

## Type constructors
Value constructors (functions) are typed as:
```
Num -> Num
```

Type constructors are written as:
```
a. (a, a)
```

The special type symbol `*` is equal to `never` in input position and `unknown` in output position.

A supertype of all functions is
```
* -> *
```

## Higher Kinded Types
Type constructors are valid types themselves.
```
type Maybe = a. {
    type: 'some'
    data: a
} + {
    type: 'none'
}
type MaybeAlias = Maybe
```

A type constructor's argument can be constrained like a value constructor's.
```
type FunctionNodeArgument = {
    Body: *
    Constraint: *
}
// For demonstration purposes. Unconstrained destructured type members would default to `*`.
type FunctionNode = { Body, Constraint }: FunctionNodeArgument. {
    parameterName: String
    constraint: Constraint
    body: Body
}
type ValueConstructor = FunctionNode {
    Body: Expression
    Constraint: TypeExpression
}
type TypeConstructor = FunctionNode {
    Body: TypeExpression
    Constraint: TypeExpression
}
```

A supertype for all type constructors is
```
* => *
```

```
type Monad = m: (* => *). {
    of: a. a -> m a
    join: a. m $ m a -> m a
    bind: a b. (a -> m b) -> m a -> m b
}
```

### Intersection of type constructors
If `a` and `b` are both type constructors, `a & b` will never be considered as
`never` by `~`. When invoked, `a & b` will invoke `a` and `b` with its argument
and return the intersection of both. `a & b` will accept the union of the inputs
accepted by `a` and `b`.
