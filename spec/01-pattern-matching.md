# Pattern Matching

(Syntax unsettled)

The `match` keyword followed by `{`, a number of clauses and finally a `}`. Each
clause is a [destructuring](00-overview#destructuring), an `=` then an expression.
```
mapMaybe = f. match {
  Some a = Some $ f a
  None = None
}
```

It evaluates to a function, so the `mapMaybe` above infers as:
```
mapMaybe: forall a b f: (a -> b). f -> (Some a + None) -> (Some b + None)
```

This currying, data-last friendly syntax subsumes:
- Declaration pattern matching
- `if` expressions
- `case _ of`
- `\case`
