-- {-# LANGUAGE UndecidableInstances #-}

module Symparsec2.Run where

import Symparsec2.Parser
import Symparsec2.Utils ( type SymbolLength )
import DeFun.Core
import GHC.TypeLits ( type Symbol )

type Run :: PParserSym r -> Symbol -> PResult r
type Run p str = p @@ ('State str (SymbolLength str) 0)
