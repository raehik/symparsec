module Data.Type.Symbol.Parser.End ( type End ) where

import Data.Type.Symbol.Parser.Internal
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

type End :: Parser () ()
type End = '(EndChSym, EndEndSym, '())

type EndCh :: ParserCh () ()
type family EndCh ch u where
    EndCh _ '() = Err (Text "expected end of string")

type EndEnd :: ParserEnd () ()
type family EndEnd u where
    EndEnd '() = Right '()

type EndChSym :: ParserChSym () ()
data EndChSym f
type instance App EndChSym f = EndChSym1 f

type EndChSym1
    :: Char -> () ~> Result () ()
data EndChSym1 ch n
type instance App (EndChSym1 ch) n = EndCh ch n

type EndEndSym :: ParserEndSym () ()
data EndEndSym msym
type instance App EndEndSym '() = EndEnd '()
