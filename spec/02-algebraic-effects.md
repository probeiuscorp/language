# Algebraic Effects

> [!NOTE]
> I'm not entirely decided on this spec.
> This documents my preferences right now.

Tilly offers commutative algebraic effects to address the configuration problem.

## Scope, motivation, and purpose

Tilly only offers commutative algebraic effects.
Koka is a significant source of inspiration:
I want to do basically everything differently than Koka.
These are some of the reasons I don't want Koka's effect system for Tilly:

- Evaluation order. I do not want an evaluation order for Tilly.
- Pleasant type signatures. That every higher-order function in Koka
must accomodate algebraic effects is unacceptable for Tilly.

Moreover, that some implementations of algebraic effects offer multiple resumption (Koka) and others don't (OCaml)
also makes me wary of trying to compete with monads.

In Tilly, algebraic effects will handle commutative effects,
while the monad family will be left to solve the rest.
Tilly's algebraic effect system then is only the `ask` effect.
Tilly then may not rightly have an algebraic effect system (if that is the case, sorry!).

> [!NOTE]
> **I don't know:** Is `ask` the only commutative algebraic effect?

The `ask` effect is offered to solve the configuration problem,
however I feel that a compelling solution to the configuration problem
would do much more than fix drilling configuration files.
I believe it would enable incredibly powerful new approaches to various problems of composition.
I believe this makes it well-worth the extra language feature.
Specifically I have FRP in mind (reactive-banana's approach),
where I envision the Events and Behaviors can be lifted into the top level.

## Interface

Algebraic effects are introduced with `ask` declarations at the top level:

```
ask offset: Nat
```

This creates two names:
- `offset` at the value level, of type `Nat ! offset`.
When demanded it will pull the corresponding value from the nearest `handle`
(note that this requires `seq` be typed slightly differently).
- `offset` at the type level, just a nominal tag.
It is neither a subtype of `Nom`, `Record`, or `HKT`.
Algebraic effects are a fourth fundamental kind of type.
Like for `HKT`, there is no value of type `offset`.

The `!` function is read as "under"
(`a -> b ! e` is read "a to b under e").
It is left associative, has higher precedence than anything but application,
and abides by the following properties:

- `a ! ⊥ = a`
- `a ! e ! e = a ! e`
- `a ! e1 ! e2 = a ! e2 ! e1`

Besides changing the type-checking and -inference rules for function application (see [Reductions](#reductions)),
the rest of the type system remains unaffected.
**Notably, the type signatures for higher order functions need not accomodate algebraic effects**:
the type of List map stays as `∀a b. (a -> b) -> List a -> List b`
(again, see [Reductions](#reductions)).

Algebraic effects are eliminated via `handle` expressions.
Here `offset3` has type `∀a. a ! offset -> a`.
Like for `match` expressions, this subject-free syntax means
'subjects' can be piped right into handle expressions.
```
offset3 = handle {
  offset = 3
```

Despite the syntactical similarity to records,
the "argument" to the `handle` keyword is _not_ a record,
but special syntax (that may not be parameterized).
Specifically, the 'keys' ("offset" here) must be valid names in scope that were created by an `ask` declaration.

## Reductions

What I've been most interested while speccing is higher-order functions like map.
Given `offset: Nat ! offset`, and `inc: Nat -> Nat ! offset`,
which should be the case?

- `map inc: List Nat -> (List Nat) ! offset` [Koka]
- `map inc: List Nat -> List (Nat ! offset)` [ideal]

There are concrete differences between the two.
In the Koka version, the length of such a list could not be found without handling `offset`.
In the ideal version, the length of such a list _can_ be found without handling `offset`.
The Koka version also needs to understand the nature of the function,
it can't just blindly move the effects out of function parameters:
`map` would have type `(a -> b ! e) -> List a -> (List b) ! e`,
while some monomorphized insertion could have type
`(a -> b ! e) -> List (a -> b ! e) -> List (a -> b ! e)`
(mapping requires performing the effects, insertion does not).
Accordingly, type signatures are [extremely painful in Koka](https://koka-lang.github.io/koka/doc/std_core_list.html).

The ideal version in contrast does not require special inference or type signatures for higher order functions,
as the effects are just propagated as part of the quantified `b`.
The ideal version is also more lazy.
If it turns out the ideal version is unsound, I will scrap algebraic effects entirely.
Koka's version is just too painful for my vision of Tilly.

From my sketching, after extending the rules of reducing applications with rule (1) and (2), I got ideal, not Koka, results:

1. If the supplied argument is under an effect,
move that effect out to the result of the application.
_TODO_: this rule fails for `const`.
Does it also fail for inserting into a collection?
There must be a better reduction out there.

```math
\frac
{\Gamma \vdash f \colon \tau_1 \to \tau_2 \text{ and } x \colon \tau_1 \;!\; \epsilon}
{\Gamma \vdash f x \colon (\tau_1 \to \tau_2)\;\tau_1 \;!\; \epsilon}
```

2. If the supplied function is under an effect,
move that effect out to the result of the application.

```math
\frac
{\Gamma \vdash f \colon (\tau_1 \to \tau_2) \;!\; \epsilon \text{ and } x \colon \tau_1}
{\Gamma \vdash f x \colon (\tau_1 \to \tau_2)\;\tau_1 \;!\; \epsilon}
```

3. If neither are under effects, use regular function type-checking.

```math
\frac
{\Gamma \vdash f \colon \tau_1 \to \tau_2 \text{ and } x \colon \tau_1}
{\Gamma \vdash f x \colon \tau_2}
```

Finally, strict evaluation needs consideration.
Luckily `seq` is a language primitive and can be retyped to `a ! e -> b -> b ! e`.
I have not given much thought to strict patterns and other approximations of `seq`,
however I'm not worried.
