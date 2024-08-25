# Data types

## Nominal data

## Records

## Typeclasses
A typeclass is declared like so:
```
export class Eq a
    areEq: a -> a -> Bool
```

Typeclass parameters can be constrained.
```
class Functor f: * => *
    fmap: forall a b. (a -> b) -> f a -> f b
```

Typeclass members can be given a default implementation.
```
class Monoid a
    mempty: a
    mappend: a -> a -> a
    mconcat: List a -> a
    mconcat = foldr mappend mempty
```

Only nominal types may be instances of typeclasses. Each nominal type may only
have one instance of each typeclass.
```
type Maybe = a. Some a + None
instance Functor Maybe
    fmap = f. match {
        Some a = Some $ f a
        None = None
    }
```

Typeclasses work mostly independently from the rest of the type system. Considering
`Monoid` again, from the rest of the type system's perspective, it's equal to:
```
mempty: forall a: Monoid. a
mappend: forall a: Monoid. a -> a -> a
mconcat: forall a: Monoid. List a -> a
```
where `Monoid` is a type which happens to have complete intersections with types
such as `List`.

There is no supertype for all typeclasses.

## Structural interactions

# Syntax

## Differentiating anonymous functions, member access and composition
First attempt to match as anonymous function:
```
| <ident> "." <whitespace> <expression>
| <ident> ":"
```
Then as member access:
```
"." <ident>
```
Otherwise, match normally (which will probably end as composition)

### Examples
```
x. x // function (f)
x.x // member access (a)
x . x // composition (c)

// f
main = none | x.
    (x, x)

// a
fmap (.x)

// c
fmap (. x)
```

## Unary operators
Unless a unary operator is separated from its expression by any whitespace, it
will be applied first, with highest precedence.

```
pow -b 3
type (Union ~k ~~k)
```

## Infix functions
If an expression starts with a newline, then an infix function and then
inline whitespace, that infix function will be ignored.

```
<newline> <symbol-identifier> <inline-whitespace>
```

```
type StructuralMaybe = a.
    + { type: 'some', data: a }
    + { type: 'none' }
```

is equal to

```
type StructuralMaybe = a. ((+) { type: 'some', data: a } { type: 'none' })
```

## Destructuring
Valid destructuring on left side

1. Constructors
```
(x:xs). undefined
(Tree x y). undefined
(Rect { x, y } (Point x' y')). undefined
```

2. Record
```
{ a, b, c }. undefined
```

Use `as` to destructure member.
```
{ a as semiminor, intercept as { x, y }}. undefined
```

Record destructuring is also used in import lists.

```
import react { lazy as lazyComponent }
```

3. Identifier
```
x
```
