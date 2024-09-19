A pure, functional programming language inspired most by Haskell, but a bit by TypeScript.

Notable differences from Haskell:
- Sound, axiomatic type system with **subtyping**, **higher kinded type** synonyms, and infix type constructors.
- A focus on anonymous functions, with concise syntax for both value functions and type constructors.
- A focus on minimizing language features. For example, do-notation as well as `in` and `where` expressions are dropped.
- (Speculative) Algebraic effects to address the configuration problem.
- ECMAScript-like module system

Notable features kept from Haskell:
- Lazy evaluation
- Typeclassing
- Curried by default

Worth mentioning:
- Emphasis on pipe operator `|` (type `∀a b. a -> (a -> b) -> b`)
- List types are `List a` instead of `[a]`
- "Batteries-included" prelude
- Point-free pattern matching by default (see Haskell's `\case`)

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
Structural typing enables convenient subtype polymorphism, and
tighter functions which don't require unused fields.
```
type Line = {
    slope: Double
    intercept: Double
}
slope l: Pick Line 'slope'. l.slope
```
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
1. Transparent RPC
2. Algebraic effects
