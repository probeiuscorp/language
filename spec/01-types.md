# Types

## Given type constructors
These are the only given type constructors, along with some compelling reasoning
that they are given.

- `&` Intersection: At least one binary function must be given to relate types.
- `¬` (U+00ac) Complement: Should be a given since it defines what `unknown` is and includes.
- `~` \<unnamed>: Should be a given since it defines how the types interact. `~` is unary and will evaluate to `unknown` if its argument is `never`, and otherwise will evaluate to `never`.

### Axiomatic type system

Union and difference can be defined with `&` and `\`:
```
// Union
(+) = a b. ¬(¬a & ¬b)
// Difference
(\) = a b. a & ¬b
```

Given some type `k`, `isNever` evaluates to a Church boolean. It is defined as follows:
```
type isNever = k. a b. (~~k & a) + (~k & b)
```

Type compatibility is thus defined:
```
type (<=) = a b. isNever $ b \ a
```

Internally the type checker applies the branching of `~` on `b \ a` to decide
whether a constraint is satisfied.

## Type constructors
Value constructors (functions) are typed as:
```
Num -> Num
```

Type constructors are written as:
```
a. (a, a)
```

The special type symbol `*` is equal to `never` in contravariant positions and `unknown` in covariant position.
It is rejected in invariant positions.

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
  of: ∀a. a -> m a
  join: ∀a. m $ m a -> m a
  bind: ∀a b. (a -> m b) -> m a -> m b
}
```

### Intersection of type constructors
If `a` and `b` are both type constructors, `a & b` will never be considered as
`never` by `~`. When invoked, `a & b` will invoke `a` and `b` with its argument
and return the intersection of both. `a & b` will accept the union of the inputs
accepted by `a` and `b`.
