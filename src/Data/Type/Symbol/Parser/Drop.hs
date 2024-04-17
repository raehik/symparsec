{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Drop where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

type PDrop :: Natural -> ParserSym' Natural ()
type PDrop n = '(DropSym, DropEndSym, n)

type Drop :: Parser Natural ()
type family Drop _ch n where
    Drop _ 0 = 'Err ('Text "can't drop 0 due to parser limitations. sorry")
    Drop _ 1 = 'Done '()
    Drop _ n = 'Cont (n-1)

type DropEnd :: ParserEnd Natural ()
type family DropEnd n where
    DropEnd 0 = 'Right '()
    DropEnd n = 'Left
      ( 'Text "tried to drop "
        :<>: 'ShowType n :<>: 'Text " chars from empty symbol")

type DropSym :: ParserSym Natural ()
data DropSym f
type instance App DropSym f = DropSym1 f

type DropSym1 :: Char -> Natural ~> Result Natural ()
data DropSym1 ch n
type instance App (DropSym1 ch) n = Drop ch n

type DropEndSym :: ParserEndSym Natural ()
data DropEndSym n
type instance App DropEndSym n = DropEnd n
