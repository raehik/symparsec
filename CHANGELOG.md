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
