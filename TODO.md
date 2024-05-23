# Symparsec to-dos
* TODO I got reify and reflect mixed up lol. fix dat
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh

## Reifying (=singling)
* expand on the SingParser1 concept
  * lets me perform the extra checks that assert that you've written a valid
    parser, on the term level
  * how do I turn a SingParser into a SingParser1 ?
* reify more parsers
  * the other takes are easy
  * `Or` will be a pain
  * `Natural` will need lots, but I've done similar in type-level-show
  * the others should be largely OK. but it's a pain

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
