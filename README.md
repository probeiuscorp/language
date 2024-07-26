A pure, functional programming language inspired by Haskell, with these design goals:
 - add structural typing
 - minimize language features
 - change some of Haskell's "missteps"

## Intentional deviations from Haskell
1. Conciser anonymous functions: `x. x` instead of `\x -> x`
2. ECMAScript-like module system
3. List types are `List a` instead of `[a]`
4. Remove do notation
5. Remove `in` and `where` expressions

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
withNewAge = update person {
    name = const 'Alex Generic I'
    birthdate = (+1)
}
```
3. Typing
```
type Person = {
    name: String
    birthdate: Date
    customerStatus: {
        joined: Date
        loyaltyPoints: Num
    }
}
```
4. Destructuring
```
type Quadratic = { a: Num, b: Num, c: Num }
y: Quadratic -> Num -> Num
y = { a, b, c } x. a * x ** 2 + b * x + c
```
### Advantages of structural typing
Structural typing enables convenient subtype polymorphism, and
tighter functions which don't require unused fields.
```
type Line = {
    slope: Num
    intercept: Num
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

## Ideas needing consideration
1. Recommended practice for orphan instances
2. Optional fields in records
3. Type union operator. `|` is taken for the pipe function, and union is not
divisible as `+` would imply.
