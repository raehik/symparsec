module Data.Type.Symbol.Parser.Parser.End ( End ) where

import Data.Type.Symbol.Parser.Types
import Data.Type.Symbol.Parser.Common ( EmitEndSym )
import GHC.TypeLits
import DeFun.Core ( type (~>), type App )

type End :: Parser () ()
type End = '(EndChSym, EmitEndSym, '())

type EndCh :: ParserCh () ()
type family EndCh ch u where
    EndCh _ '() = Err (EBase "End" (Text "expected end of string"))

type EndChSym :: ParserChSym () ()
data EndChSym f
type instance App EndChSym f = EndChSym1 f

type EndChSym1
    :: Char -> () ~> Result () ()
data EndChSym1 ch n
type instance App (EndChSym1 ch) n = EndCh ch n
