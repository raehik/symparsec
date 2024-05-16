{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes #-}

module Symparsec.Reify where

import Symparsec.Parser
import Symparsec.Run
import Data.Kind ( Type, Constraint )

import Symparsec.Parser.Take
import GHC.TypeNats
import GHC.TypeLits
    ( Symbol, KnownSymbol, symbolVal', UnconsSymbol, ConsSymbol, KnownChar )
import GHC.Exts ( Proxy#, proxy# )

{-
class Reify (p :: Parser s r) where
    type ReifyS  p :: Type
    type ReifySC p (s' :: s) :: Constraint
    reifyS' :: ReifySC p s' => Proxy# s' -> ReifyS p

reifyS
    :: forall {s} {r} (p :: Parser s r) (s' :: s)
    .  (Reify p, ReifySC p s') => Proxy# s' -> ReifyS p
reifyS = reifyS' @s @r @p

-- | Reify the initial state of the given parser.
reifySInit
    :: forall {s} {r} (p :: Parser s r)
    .  (Reify p, ReifySC p (P3 p)) => ReifyS p
reifySInit = reifyS @p (proxy# @(P3 p))

type family P3 (p :: Parser s r) where P3 ('Parser pCh pEnd s) = s

instance Reify (Take' n) where
    type ReifyS (Take' n) = (Natural, String)
    type ReifySC (Take' n) s =
        (KnownNat (Fst s), KnownSymbol (SymbolFromChars (Snd s)))
    reifyS' (_ :: Proxy# s') =
        ( natVal' (proxy# @(Fst s'))
        , symbolVal' (proxy# @(SymbolFromChars (Snd s'))))

type family Fst ab where Fst '(a, b) = a
type family Snd ab where Snd '(a, b) = b
-}

{-
reifyTakeS
    :: forall {(s :: (Natural, [Char]))
    .  (s ~ '(n', chs), KnownNat n, KnownSymbol (SymbolFromChars chs))
    => (Natural, String)
-}

{-
type family SymbolFromChars (chs :: [Char]) :: Symbol where
    SymbolFromChars '[]        = ""
    SymbolFromChars (ch : chs) = ConsSymbol ch (SymbolFromChars chs)

type CharsFromSymbol :: Symbol -> [Char]
type CharsFromSymbol sym = CharsFromSymbol' (UnconsSymbol sym)

type family CharsFromSymbol' (msym :: Maybe (Char, Symbol)) :: [Char] where
    CharsFromSymbol' Nothing           = '[]
    CharsFromSymbol' (Just '(ch, sym)) =
        ch : CharsFromSymbol' (UnconsSymbol sym)
-}

{- Notes:

* We can write a state reifier and automatically reify initial state. But it's
  (expectably) more complicated than just repeating the initial state on the
  term level manually. And this reifier is meant to be simple.
-}
