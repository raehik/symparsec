# Symparsec to-dos
## Proofs (tests?)
* `Isolate n TakeRest` is equivalent to `Take n`

## Combinators
* `Choice :: [PParser a] -> PParser a`
* various from parser-combinators, megaparsec (e.g. `sepBy`)

## Example uses of Symparsec to write, present
### This StackOverflow one from 2022-06
https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell

### generic-data-functions
Fiddly and awfully abstract. WIP.

### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.
