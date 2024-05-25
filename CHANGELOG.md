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
