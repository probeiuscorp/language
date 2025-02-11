# Pattern Matching

Pattern matching is provided via a point-free expression.

The `match` keyword followed by `{`, a number of clauses and finally a `}`. Each
clause is a [destructuring](00-overview#destructuring), an `=` then an expression.
```
mapMaybe = f. match
  Some a = Some $ f a
  None = None
```

It evaluates to a function, so the `mapMaybe` above infers as:
```
mapMaybe: ∀a b. (a -> b) -> ((Some a -> Some b) & (None -> None))
```

To match on a subject, use the pipe function.
```
user.accountProvider | match
```

This currying, data-last friendly syntax subsumes:
- Declaration pattern matching
- `if` expressions
- `case _ of`
- `\case`
