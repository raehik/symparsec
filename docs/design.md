# Symparsec design notes
This document simplifies some types and kinds for clarity.

## Parser design
[hackage-flatparse]: https://hackage.haskell.org/package/flatparse

A Symparsec parser is simply a `s -> (a, s)` function, reflected and
defunctionalised (see [What is
defunctionalization?](#what-is-defunctionalization) below for an explanation).
Parser state is comparable to common parser libraries:

```haskell
data State = State
  { remaining :: Symbol  -- | Remaining input.
  , length    :: Natural -- | Remaining permitted length.
  , index     :: Natural -- | Index in the input string (for errors)
  }
```

The `length` field permits much simpler parser design for parsers that act on a
substring of the input. ([flatparse][hackage-flatparse] was an inspiration
here.)

### Naming conventions
Parsers are `State ~> Reply a` defunctionalization symbols.
For parsers of this form, I omit the common `Sym` suffix that defun uses to
denote defunctionalization symbols. e.g.

```haskell
type Take :: Natural -> State ~> Reply a
data Take n s
instance App (Take n) s = Take' n s -- where Take' is a type family
```

(Morally, this is because `type Parser a = State ~> Reply a`, and one can
consider parsers to be opaque in how they're actually run.)

If you compose parsers monadically, you need further defunctionalization.
In such cases, the `Sym` suffix returns. e.g.

```haskell
type TakeSym :: Natural ~> State ~> Reply a
data TakeSym n
instance App TakeSym n = Take n
```

### Symparsec 1.0
In my original design, I offloaded much fiddly work to the parser runner.
Parsers were only permitted to consume a character at a time.
Complex parsers were achieved via funky state handling, which the runner
threaded through.

I designed it this way because it seemed most safe and straightforward.
However, the state manipulation was a source of great complexity, and it
completely prevented writing mutually-recursive parsers.

Symparsec 2.0 did away with the parser runner: now parsers are `s -> (a, s)`
monadic state functions, and running them is no more than function application.
It's more powerful, and plenty of parsers are simplified, but it's intimidating.

## What is defunctionalization?
[hackage-defun-docs]: https://hackage.haskell.org/package/defun-0.1/docs/DeFun.html

In term-level Haskell, we often pass functions around as arguments e.g. `fmap
(+1) xs`. You _can not_ do the same on the type level: type families must always
be fully applied (think like type synonyms). This hugely limits composability of
type-level Haskell programs.

Defunctionalization is the process of replacing functions with data. Data can be
passed around without issue. When you wish to access the function it represents,
you do so with an "apply" helper. In type-level Haskell, this whole concept maps
to _replacing type families with type constructors_. Critically, type
constructor _are_ partially-applicable (e.g. `Either :: Type -> Type -> Type`).

There are a handful of defunctionalization libraries on Hackage. Each one does
defunctionalization a slightly different way. Symparsec uses phadej's `defun`
package, because the author found it easiest to understand. See [`module
DeFun`][hackage-defun-docs] on Hackage for excellent, beginner-friendly
explanations.

*(Note that there are proposals to support partially-applicable type families in
GHC Haskell. But the author doesn't expect them to come for many years.)*
