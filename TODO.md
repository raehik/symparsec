# symbol-parser to-dos
## Combinators
None in particular. Probably a lot of easy ones though, please consider making
an issue or PR if you have one in mind.

## Assorted
* TODO can we replace ParserEnd parts with an enforced Eot??!!! lmao maybe
  * hmmm idk. I think the ParserEnd separation just makes it safer, like we
    can't ever return a Cont at the end now which just makes things easier to
    track.
* implement this parser (2022-06)
  https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell
