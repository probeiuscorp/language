<img alt="Tilly Logo" src="tilly-logo-full.png" />

A pure, functional programming language inspired most by Haskell, but a bit by TypeScript.

Notable features and differences from Haskell:
- **Complement types**, as the (¬) function. For example:
```
data Errors = NotProvided + NotInteger + NotInRange
replaceLeft = f. match
  (Left e) = f e
  right = right
defaultToZero: ∀e. Either e Int -> Either (e & ¬NotProvided) Int
defaultToZero = replaceLeft $ match
  NotProvided = Right 0
  left = left
```
From this function and the intersection (&), the type checker is sound and [axiomatic](spec/01-types.md#given-type-constructors).
- Subtyping:
```
getRight: ∀a. Either ⊥ a -> a
getRight = match
  Left e -> e  // Since `e` is ⊥, it is compatible with `a`
  Right a -> a
```
- Expression oriented and point-free. No special syntax in declarations.
Anonymous function expressions are focused, with concise syntax for functions as well as for [pattern matching](spec/01-pattern-matching.md):
```
id = x. x
type id = x. x
S = x y z. x z $ y z
type S = x y z. x z $ y z
mapMaybe = f. match
  (Some x) = Some $ f x
  (None) = None
```
- ECMAScript-like [module system](spec/01-modules.md).

Notable features kept from Haskell:
- Lazy evaluation
- Typeclassing
- Curried by default

Some changes from Haskell:
- Emphasis on pipe operator `|` (type `∀a b. a -> (a -> b) -> b`)
- List types are `List a` instead of `[a]`
- Do notation dropped

## Non-Goals

### Respecting precedent

For example, `Maybe`'s constructors are `Some` and `None`. I like `Maybe` more than
`Option` since it is more distinctive (more searchable, filters down autosuggest more).
I like `Some` and `None` and I dislike `Just` and `Nothing`.

## Structural typing
Add structurally typed records inspired by TypeScript.

1. Syntax
```
person = {
  name = 'Alex Generic'
  birthdate = Date 1821 4 20
  customerStatus = {
    joined = Date 2024 02 10
    loyaltyPoints = 140
    balance = 12310
  }
}
```
2. Reading and updating
```
ageWhenJoined = person.customerStatus.joined - person.birthdate
// Speculative
withNewAge = person | update {
  name = const 'Alex Generic I'
  birthdate = (+1)
  customerStatus = update {
    loyaltyPoints = (+30)
  }
}
```
3. Typing
```
type Person = {
  name: String
  birthdate: Date
  customerStatus: {
    joined: Date
    loyaltyPoints: Int
  }
}
```
4. Destructuring
```
type Quadratic = { a: Double, b: Double, c: Double }
y: Quadratic -> Double -> Double
y = { a, b, c } x. a * x ** 2 + b * x + c
```
### Advantages of structural typing
Fun with unions, intersections and differences
```
type StructuralMaybe = a. {
  type: 'some'
  data: a
} + {
  type: 'none'
}
type StructuralSome = a. StructuralMaybe a & { type: 'some' }
type StructuralNone = StructuralMaybe () \ StructuralSome ()
```
Top and bottom too.
```
type Id = a. (a & unknown) + never
```

Time complexity for property access is currently undefined.

## Concerns
1. Recommended practice for orphan instances
2. Optional fields in records
3. Type union operator. `|` is taken for the pipe function, and union is not
divisible as `+` would imply.

## Ideas
1. Algebraic effects
