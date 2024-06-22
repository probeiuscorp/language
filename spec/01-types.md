# Types

## Type constructors
Value constructors (functions) are typed as:
```
Num -> Num
```

Type constructors are typed as:
```
a. (a, a)
```

Parametric polymorphism using both:
```
type Id = a. a -> a
```

The special type symbol `*` is equal to `unknown` in input position and `never` in output position.

The supertype of all functions is
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
// For demonstration purposes. Unconstrained destructured type members default to `*`.
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
    Constraint: TypeConstraintExpression
}
```

Type constructor constraints can be TypeExpressions or a type constructor.

The superconstraint of all type constructors is
```
* => *
```

```
type Monad = m: * => *. {
    of: a. a -> m a
    join: a. m $ m a -> m a
    bind: a b. (a -> m b) -> m a -> m b
}
```
