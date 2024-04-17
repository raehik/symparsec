# symbol-parser
Type level string (`Symbol`) parser combinators.

This library is effectively made possible by `UnconsSymbol :: Symbol -> Maybe
(Char, Symbol)` introduced in GHC 9.2. A big thank you to all GHC developers!!!

## Design
[defun-core-hackage]: https://hackage.haskell.org/package/defun-core

```haskell
type Parser s r = Char -> s -> Result s r
data Result s r = Cont s | Done r | Err ErrorMessage
```

`Symbol`s are parsed `Char` by `Char`. For each `Char`, the parser is called
with the `Char` in question and the current parser state. Parsing continues
based on the result.

We pass parsers around via defunctionalization symbols. The plumbing here is
provided by Oleg's fantastic [defun-core][hackage-defun-core].

I try to keep the internals as simple as possible. Hopefully, this library looks
like any other parser combinator, except reflected up to the type level.

## Why?
Via `GHC.Generics`, we may inspect Haskell data types on the type level.
Constructor names are `Symbols`. Ever reify these, then perform some sort of
checking or parsing on the term level? Now you can do it on the type level
instead. Catch bugs earlier, get faster runtime.

## I want to help
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
