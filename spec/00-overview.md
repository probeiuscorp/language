# Data types

## Nominal data

## Records

## Typeclasses

## Structural interactions

# Syntax

## Differentiating anonymous functions, member access and composition
First attempt to match as anonymous function:
```
| <ident> . <whitespace> <expression>
| <ident> :
```
Then as member access:
```
. <ident>
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
