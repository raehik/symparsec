# Symparsec to-dos
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh

## Reifying
### Automatic return type demoter
I think having the user demote the return type themselves is appropriate -- most
general, reminds what sort of library they are using -- but we could write a
type class that demotes automatically, using base types like `String` and
`Either` and `Tuple2`. Then if you don't need manual demotion, you can just plug
in the type class method and it should figure out what to do.

## Combinators
None in particular. Probably a lot of easy ones though, please consider making
an issue or PR if you have one in mind.

## Examples
### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.

### generic-data-functions
Fiddly and awfully abstract. WIP.

## Assorted
* TODO can we replace ParserEnd parts with an enforced Eot??!!! lmao maybe
  * hmmm idk. I think the ParserEnd separation just makes it safer, like we
    can't ever return a Cont at the end now which just makes things easier to
    track.
* implement this parser (2022-06)
  https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell
