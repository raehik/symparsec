module Symparsec.Parser.End ( End, sEnd ) where

import Symparsec.Parser.Common
import DeFun.Core ( Con1, con1 )
import Singleraeh.Tuple ( SUnit(..) )
import Singleraeh.Either ( SEither(..) )
import GHC.TypeLits ( symbolSing )
import TypeLevelShow.Doc ( singDoc )

-- | Assert end of symbol, or fail.
type End :: Parser () () ()
type End = 'Parser
    (FailChSym "End" (Text "expected end of string"))
    (Con1 Right)
    '()
    (Con1 Right)

sEnd :: SParser SUnit SUnit SUnit End
sEnd = SParser (failChSym symbolSing singDoc) (con1 SRight) SUnit (con1 SRight)

-- TODO orphan instance. if I need to, make some dumb type family idk.
instance SingParser End where
    type PS0 End = SUnit
    type PS  End = SUnit
    type PR  End = SUnit
    singParser' = sEnd
