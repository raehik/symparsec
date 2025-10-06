## 2.0 (Unreleased)
Full rewrite.

* parsers are now much more general: mutually-recursive parsers are game
  * added an example parser for a simple expression AST
* added parsers matching `Functor`, `Applicative`, `Monad` type class methods

Simple parsers written with the provided combinators should still function the
same, or with minimal changes.

## 1.1.1 (2024-06-15)
* add `Apply` combinator (effectively `fmap`)
* add some more runners and utils (handy for generic-data-functions)

## 1.1.0 (2024-06-01)
* add `While` combinator
* add `Count` combinator
* re-add `:<|>:` re-export in `Symparsec.Parsers`

## 1.0.1 (2024-05-27)
* add `TakeRest` combinator
* re-add `:<|>:` combinator with more accurate behaviour clarification

## 1.0.0 (2024-05-25)
* small rewrite, changing how `Done` works (now non-consuming)
* single all parsers
  * ...except `:<|>:`, which is disabled for now due to complexity

## 0.4.0 (2024-05-12)
* rebrand from symbol-parser to Symparsec
* rename `Drop` -> `Skip` (more commonly used for monadic parsers)
* document parsers
* provide fixity declarations for infix binary combinators

## 0.3.0 (2024-04-20)
* add new parsers: `Take`, `:<|>:`
* tons of cleanup, renaming (`RunParser` -> `Run`)
* add handful of tests (via type-spec)

## 0.2.0 (2024-04-19)
* add two more combinators: `End`, `Literal`
* remove some old code (`Data.Type.Symbol`, `Data.Type.Symbol.Natural`)
* fix base lower bound (at least base-4.16, == GHC 9.2)
* style: don't tick promoted constructors unless necessary for disambiguation

## 0.1.0 (2024-04-17)
Initial release.

  * basic combinators: `Drop`, `Isolate`, `NatHex` (etc.), sequencing
  * acceptable error messages
