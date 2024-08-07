# Data types

## Nominal data

## Records

## Typeclasses

## Structural iteractions

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
