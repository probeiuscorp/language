# Data types

## Nominal data

Nominal types can be created using `data` declarations.
```
data ANominal
```

Nominal types can have a body, in which case unknown capitalized identifiers are
created as new nominal types. The left hand identifier will no longer be created
as a nominal type but rather as a regular type, but marked so that a typeclass
can be defined on it.
```
data Maybe = a. Some a + None
```

If a data declaration is exported, so are all the created nominal types.

## Records

## Typeclasses
A typeclass is declared like so:
```
export class Eq a where
  areEq: a -> a -> Bool
```

Typeclass parameters can be constrained.
```
class Functor f: (* => *) where
  fmap: (a -> b) -> f a -> f b
```

Typeclass members can be given a default implementation.
```
class Monoid a where
  mempty: a
  mappend: a -> a -> a
  mconcat: List a -> a
  mconcat = foldr mappend mempty
```

Only nominal types may be instances of typeclasses. Each nominal type may only
have one instance of each typeclass.
```
data Maybe = a. Some a + None
instance Functor Maybe where
  fmap = f. match
    Some a = Some $ f a
    None = None
```

Typeclasses work mostly independently from the rest of the type system. Considering
`Monoid` again, from the rest of the type system's perspective, it's equal to:
```
mempty: ∀a: Monoid. a
mappend: ∀a: Monoid. a -> a -> a
mconcat: ∀a: Monoid. List a -> a
```
where `Monoid` is the XOR of its instances.

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
Unary operators are applied with highest precedence.
They can be either prefix or postfix operators.

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

## Multiline operator shorthand

If an expression begins with an infix operator, and then the line is ended, the
offside-rule will be used to find the arguments which will be folded by that
operator.

```
errorMessage = <>
  "Failed to get user data.\n"
  "Check your internet connection.\n"
  "HTTP " <> errno <> "\n"
```

This also enables something akin to `do`-notation, but more expression-oriented:
```
main = >>
  putStr "Hello, what is your name? "
  getLine
  /* I could apply a function to this whole function */ ln. putStrLn $
    "Hello " ++ ln ++ "!"
```

**TODO**: How does this work with expressions? The simplest would be allowing backtick infixed,
and it *would* allow arbtrary expressions, but that leaves a *lot* of diff churn
and clumsiness.

```
type StructuralMaybe = a. (a b. ¬(¬a & ¬b)) | union. `union`
  { type: 'some', data: a }
  { type: 'none' }
```
