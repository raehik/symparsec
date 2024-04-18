{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Symbol.Parser.Drop ( type Drop ) where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

type Drop :: Natural -> Parser Natural ()
type Drop n = '(DropChSym, DropEndSym, n)

type DropCh :: ParserCh Natural ()
type family DropCh _ch n where
    DropCh _ 0 = Err (Text "can't drop 0 due to parser limitations. sorry")
    DropCh _ 1 = Done '()
    DropCh _ n = Cont (n-1)

type DropEnd :: ParserEnd Natural ()
type family DropEnd n where
    DropEnd 0 = Right '()
    DropEnd n = Left
      ( Text "tried to drop "
        :<>: ShowType n :<>: Text " chars from empty symbol")

type DropChSym :: ParserChSym Natural ()
data DropChSym f
type instance App DropChSym f = DropChSym1 f

type DropChSym1 :: Char -> Natural ~> Result Natural ()
data DropChSym1 ch n
type instance App (DropChSym1 ch) n = DropCh ch n

type DropEndSym :: ParserEndSym Natural ()
data DropEndSym n
type instance App DropEndSym n = DropEnd n
