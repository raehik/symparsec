module Symparsec.Parser.End ( End, sEnd ) where

import Symparsec.Parser.Common
import DeFun.Core ( Con1, con1 )
import Singleraeh.Tuple ( SUnit(..) )
import Singleraeh.Either ( SEither(..) )
import GHC.TypeLits ( symbolSing )
import TypeLevelShow.Doc ( singDoc )

-- | Assert end of symbol, or fail.
type End :: Parser () ()
type End = 'Parser
    (FailChSym "End" (Text "expected end of symbol")) (Con1 Right) '()

sEnd :: SParser SUnit SUnit End
sEnd = SParser (failChSym symbolSing singDoc) (con1 SRight) SUnit
