{-# LANGUAGE UndecidableInstances#-}

{- | Experiments.

Turns out we can't write recursive parsers. But 'P.Apply' can help us write
handier parsers.
-}

module Symparsec.Example.Expr where

import Numeric.Natural
import Symparsec.Parsers qualified as P
import Symparsec.Parser.While.Predicates qualified as P
import Symparsec.Parser.Apply qualified as P
import DeFun.Core
import DeFun.Function

data Expr
  = ELit Lit
  | EBOp Expr BOp Expr

data BOp = Plus

data Lit
  = LNat Natural

type PLit  = PLNat
type PLNat = Con1 LNat P.:<$>: P.While P.IsHexDigitSym P.NatHex
type PELit = Con1 ELit P.:<$>: PLit
type PEBOp = Curry3Sym (Con3 EBOp) P.:<$>: (PExpr P.:<*>: PBOp P.:<*>: PExpr)
type PExpr = PELit
--type PExpr :: PParser (P.OrS (Maybe Natural) (Maybe Natural)) Expr
--type PExpr = FromEitherSym P.:<$>: (PELit P.:<|>: PEBOp)
type PBOp  = ConstSym1 Plus P.:<$>: P.Literal "+"

type Curry3Sym
    :: (a ~> b ~> c ~> r)
    -> ((a, b), c)
    ~> r
data Curry3Sym f abc
type instance App (Curry3Sym f) '( '(a, b), c) = f @@ a @@ b @@ c

type FromEither :: Either a a -> a
type family FromEither eaa where
    FromEither (Right a) = a
    FromEither (Left  a) = a

type FromEitherSym :: Either a a ~> a
data FromEitherSym eaa
type instance App FromEitherSym eaa = FromEither eaa
