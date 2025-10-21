# Symparsec to-dos
* custom state necessitates `TypeAbstractions` (GHC 9.8) sometimes: clarify

## Proofs (tests?)
* `Isolate n TakeRest` is equivalent to `Take n`

## Combinators
* `Choice :: [PParser a] -> PParser a`
* various from parser-combinators, megaparsec (e.g. `sepBy`)

## Example uses of Symparsec to write, present
### generic-data-functions
Fiddly and awfully abstract. WIP.

### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.
