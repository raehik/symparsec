module Symparsec.Parser.End where

import Symparsec.Parser.Common
import DeFun.Core
import Singleraeh.Tuple ( SUnit(..) )
import Singleraeh.Either ( SEither(..) )

-- | Assert end of symbol, or fail.
type End :: PParser () ()
type End = 'PParser EndChSym (Con1 Right) '()

sEnd :: SParser SUnit SUnit End
sEnd = SParser sEndChSym (con1 SRight) SUnit

instance SingParser End where
    type PS  End = SUnit
    type PR  End = SUnit
    singParser' = sEnd

-- it'd be nice to just reuse FailChSym here but we get told off for writing an
-- orphan instance. fair enough, write this instead
type EndChSym :: ParserChSym s r
data EndChSym f
type instance App EndChSym f = EndChSym1 f

type EndChSym1 :: ParserChSym1 s r
data EndChSym1 ch s
type instance App (EndChSym1 ch) s =
    Err (EBase "End" (Text "expected end of string"))

sEndChSym :: SParserChSym ss rr EndChSym
sEndChSym = Lam2 $ \_ch _s -> SErr singE
