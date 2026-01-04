<img alt="Tilly Logo" src="tilly-logo-full.png" />

A pure, functional programming language inspired most by Haskell, but a bit by TypeScript and Elixir.

# What makes Tilly special?

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
