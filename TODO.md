# Symparsec to-dos
* think I use reify vs. reflect properly but not 100% sure
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh

## Design flaws
### Must unwrap combinators for reifying
See `Isolate'`. We could resolve this by defunctionalizing _again_, but it's
actually just a style matter, since the type family has a single, non-stuckable
equation. So for now, I think this is easiest, even if it's annoying. But if
there's another way that doesn't add a ton more boilerplate, heck yeah.

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
