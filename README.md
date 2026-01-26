<img alt="Tilly Logo" src="tilly-logo-full.png" />

A pure, functional programming language inspired most by Haskell, but a bit by TypeScript and Elixir.

# What defines Tilly?

1. **Complement types.**
Tilly has a set-theoretic type system, with complement and intersection types (union comes for free).
Together with a function to inspect whether any given type is null/empty,
Tilly's type system is [axiomatic](./spec/01-types.md#axiomatic-type-system).
Tilly's type checker _is_ essentially that inspecting function — TypeScript's conditional types thus come for free.

2. **Expression-oriented.**
Tilly has no statements and all its declarations are single-purpose.
Functions, conditionals, pattern matching, and type annotations are all expressions and so can be manipulated like expressions.

3. **Line-oriented syntax.**
I optimized Tilly's syntax to produce the cleanest Git diffs and avoid unnecessary diff churn and merge conflicts.
Not allowing trailing commas is a classic example of diff churn.
Tilly tries to go a step farther so that there aren't silly reindents, churning imports, or misalignment due to operators.

4. **Anonymous records + nominal types.**
Tilly offers two kinds of data types:
anonymous records which will be very familiar to TypeScript users,
and nominal types which will be very familiar to Haskell users.
Neither are exactly as from prior art, being designed to play nice with both the typeclassing sort of polymorphism
as well as highly expressive (such as parameterized + mutually recursive) data modeling problems.

5. **(Commutative) algebraic effects.**
Inspired by React Context and offered to address the configuration problem,
"ask" effects allow highly nested code to implicitly receive parameters from high up in the evaluation tree.
I hate how Koka does algeraic effects so Tilly only offers [_commutative_ algebraic effects](./spec/02-algebraic-effects.md) — there is **no** evaluation order in Tilly.
Non-commutative effects remain the business of monads.

6. **Preludes and module system.**
OK, this one is basically more on point (3).
TypeScript's module system is a never ending source of diff churn:
importing small utils followed by dropping unused imports.
In what world is `useState` going to refer to _anything_ in my frontend code except for the one from `react`?
Tilly allows users to create scoped, custom preludes.
Anything in a prelude will be immediately available to all the source files it is scoped for.

7. **Non-stratified typeclasses**.
In Haskell, typeclasses are `Constraint`s, not `Type`s at all.
In Tilly typeclasses are just types like any other.
Simply letting a typeclass be the union of all its instances though [doesn't work](./spec/02-typeclassing.md#value),
instead a typeclass must be the **XOR** (yes, Boolean XOR!) of all its instances.

## Comparison with TypeScript

This section is written for readers familiar with TypeScript but not Haskell.
As such, many of the powerful ideas and features shown here are not unique to Tilly but come from Tilly's Haskell roots.
Where they are unique to Tilly, it will be noted.

- As a pure functional programming language,
Tilly has no statements, evaluation order, or side effects
(note that lacking _side effects_ does **not** mean Tilly can't have _external effects_).
All code is expressions whose only utility is what they evaluate to — maximizing locality.

- Function composition operator, pipe operator, and custom operators.

- Tilly has pattern matching.
In TypeScript if-else ladders are the most conservative, simple, and standard way to consume discriminated unions,
however they are far from desirable for the reasons that follow.
Proper pattern matching syntax is much friendlier to Git and other line operations.
See [pattern matching](./spec/01-pattern-matching.md) for motivation and examples.
  1) if-else ladders are statements, not expressions;
  2) have to repeat the return keyword;
  3) either extra lines are wasted on closing braces or the first condition is not aligned with the following conditions;
  4) either the final clause goes in an else and so the condition is not shown in the code,
or the final clause gets its own else-if and so
either there is no else and any extensions to the union later will silently be ignored
or a `never` check must be put in the else (bothersome).

- Tilly has complement types.
This means a function could require its input be anything but a number.
That may not sound too useful but once you intersect that complement with a domain it becomes very useful indeed.

  1) A simple use for complement types is error handling:
start with data like `Either MyErrorType MyDesiredData` where `MyErrorType` is the union of a bunch of different error conditions.
Now make a bunch of functions that handle individual error variants.
Using complement types you can say that these functions will then
give you an `Either` with the handled variants having been intersected _out_ of the type.
Compose functions to handle every variant of error,
and if you did it right you'll have intersected _out_ every error variant.
You now have `Either never MyDesiredData`, where you can freely pluck out the `MyDesiredData`.
  2) XOR types are another natural use for complement types.
In TypeScript `a | b` will allow the types where the value is, well, both `a` and `b`.
XOR types in Tilly (`a ^ b`) permit the value to be either an `a` or a `b`,
but do not permit the value to be possibly either.
This is an uncommon enough thing to do you may never have felt its absence in TypeScript,
but when you do need XOR types, they are incredibly expressive.

- Tilly has Higher Order Types.
TypeScript's type system is itself a pure functional language,
and generics are actually just functions between types (`type Id<T> = T`).
Higher Order Types are akin to first-class functions, but for the type level.
In languages that support Higher Order Types,
generics (like that `Id` type) are valid types in and of themselves:
I could write `Id<Id>` and get `Id`.
Higher Order Types are useful for describing patterns like the functor and monad.

- Tilly's records are comparable to but different from TypeScript objects.
Notably records do not offer a way to inspect a record for all the fields it contains (no `Object.keys`)
and do not allow arbitrary values to be inserted into records (can't be used like a dictionary).
Tilly's records are more like named tuples.

## Comparison with Haskell

Syntax is the biggest way Tilly differentiates itself from Haskell.
Tilly only has (anonymous) function expressions, so no moving parameters to the left hand side of an equal sign.
Tilly also has only construct for pattern matching, `match` expressions (same as `\case`).
There are no patterns in definitions, `if` expressions, or `case` expressions.

- Anonymous function expressions.
- Expressions-based declaration of nominal data types (ADTs)
- Point free pattern matching
- Subtyping

# Status of the project

Currently the compiler parses most of the language and will generate runtime code for a small number of features.
The simplest runtime system does exist, and line-based stdin/stdout monadic IO works.
A VSCode extension for simple syntax highlighting is available.

These things are not done:

- Typechecker, especially anything relating to quantified types
- Module system
- Runtime generation of non-core language features like records
- Garbage collector
- Anything resembling a standard library

# Tour

What does Tilly code look like?

## External effects

Tilly is a pure language so there are no side effects.
However, that doesn't mean Tilly can't have external effects.
Tilly uses monadic IO, where your code creates IO expressions the runtime then interprets and executes.
The following defines `main` to be the IO expression requesting that "Hello, world" be put on stdout.

```tilly
main = putStrLn "Hello, world"
```

These IO expressions are manipulated like any other,
with the specific interface being the monad interface.
The `*>` function yields a new IO expression that requests executing the LHS before the RHS.

```haksell
main =
  *> putStrLn "Hello..."
  *> putStrLn "world!"
```

The `>>` function yields a new IO expression that, after executing the LHS,
will take the result of the LHS and apply it to the RHS function (here `line. putStrLn line`),
getting a new IO expression it will then execute.

```tilly
main = getLine >> line. putStrLn line
-- or...
main = getLine >> putStrLn
```

Because Tilly is a lazy language,
all expressions can be recursively defined, not just functions.
This `main` will echo forever.


```tilly
main = (getLine >> putStrLn) *> main
```

Put together, we can express any kind of external effect we want.

```tilly
-- prompt has type:
-- String -> (String -> Either String a) -> IO a
prompt = promptMessage parse.
  promptAgain = >>
    getLine
    parse > match
      (Left errorMessage). *>
        putStrLn $ "Please enter a valid value: " <> errorMessage
        promptAgain
      (Right goodValue). of goodValue
  putStrLn promptMessage *> promptAgain

-- main has type:
-- IO ()
main = >>
  sequence (prompt "First name" Right, prompt "Last name" Right)
  (first, last). putStrLn $
    <> "Good to meet you, "
    <> first <> " " <> last <> "!"
  -- join takes IO (IO a) -> IO a
  -- Here we return an IO action based off the user's response
  join $ prompt "Rate this Italian salad" $ toLower > match
    | anyp [(== "good"), (== "ok")].
      Right $ putStrLn "Awesome! I made it just for you!"
    | anyp [(== "not good"), (== "bad")].
      Right $ putStrLn "Snap! I'll try harder for next time."
    input. Left $ "I don't understand " <> input
```
