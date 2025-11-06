# Symparsec to-dos
* custom state necessitates `TypeAbstractions` (GHC 9.8) sometimes: clarify

## Proofs (tests?)
* `Isolate n TakeRest` is equivalent to `Take n`

## Combinators
* `Fold`. `Foldr`, I guess? Idk.
* various from parser-combinators, megaparsec. find out the most common/used

## Example uses of Symparsec to write, present
### Generics, parsing constructor & field names
* generic-data-functions: done for 1.0. fiddly and awfully abstract. TODO
* aeson: super real world! do constructor & field name parsing, instead of
  `constructorTagModifier` and such. pretty daunting though (nasty generics)

### JSON parser
Big, real world, funny. Roll it by hand (Aeson's parsing is very complex).

## Completed examples
Just to remind myself what is already attempted.

### Expression parser
Not very well-written, mind you, but it works. I could probably write a
`MakeExprParser` like in megaparsec, but it's terribly complex.

### Format string parser
Directly from typelits-printf, minimal changes.

### Something that uses multi-line `Symbol`s
Threw into the expression parser. 9.12 exclusive due to `MultinelineStrings`.
