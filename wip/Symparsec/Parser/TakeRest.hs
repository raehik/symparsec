{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.TakeRest where

import Symparsec.Parser.Common
import Singleraeh.Symbol ( RevCharsToSymbol, revCharsToSymbol )
import Singleraeh.List ( SList(..) )
import Singleraeh.Either ( SEither(..) )
import GHC.TypeLits hiding ( ErrorMessage(..) )
import DeFun.Core

-- | Return the remaining input string.
type TakeRest = 'PParser TakeRestChSym TakeRestEndSym '[]

sTakeRest :: SParser (SList SChar) SSymbol TakeRest
sTakeRest = SParser sTakeRestChSym sTakeRestEndSym SNil

instance SingParser TakeRest where
    type PS TakeRest = SList SChar
    type PR TakeRest = SSymbol
    singParser' = sTakeRest

type TakeRestChSym :: ParserChSym [Char] Symbol
data TakeRestChSym f
type instance App TakeRestChSym f = TakeRestChSym1 f

type TakeRestChSym1 :: ParserChSym1 [Char] Symbol
data TakeRestChSym1 ch chs
type instance App (TakeRestChSym1 ch) chs = Cont (ch : chs)

sTakeRestChSym :: SParserChSym (SList SChar) SSymbol TakeRestChSym
sTakeRestChSym = Lam2 $ \ch chs -> SCont $ SCons ch chs

type TakeRestEndSym :: ParserEndSym [Char] Symbol
data TakeRestEndSym chs
type instance App TakeRestEndSym chs = Right (RevCharsToSymbol chs)

sTakeRestEndSym :: SParserEndSym (SList SChar) SSymbol TakeRestEndSym
sTakeRestEndSym = Lam $ \chs -> SRight $ revCharsToSymbol chs
