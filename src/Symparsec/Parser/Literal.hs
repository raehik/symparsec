{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Literal where

import Symparsec.Parser.Common
import GHC.TypeLits hiding ( ErrorMessage(..) )
import TypeLevelShow.Utils ( ShowChar, sShowChar )
import Data.Type.Equality
import DeFun.Core
import Singleraeh.Tuple
import Singleraeh.Either
import Singleraeh.Maybe
import Singleraeh.Symbol
import Unsafe.Coerce
import TypeLevelShow.Doc

-- | Parse the given 'Symbol'.
type Literal :: Symbol -> PParser Symbol ()
type Literal str = 'PParser LiteralChSym LiteralEndSym str

sLiteral :: SSymbol str -> SParser SSymbol SUnit (Literal str)
sLiteral str = SParser sLiteralChSym sLiteralEndSym str

instance KnownSymbol str => SingParser (Literal str) where
    type PS (Literal str) = SSymbol
    type PR (Literal str) = SUnit
    singParser' = sLiteral SSymbol

type LiteralCh ch str = LiteralCh' ch (UnconsSymbol str)
type family LiteralCh' ch str where
    LiteralCh' ch (Just '(ch, str))     = Cont str
    LiteralCh' ch (Just '(chNext, str)) = Err (EWrongChar chNext ch)
    LiteralCh' ch Nothing               = Done '()

type EWrongChar chNext ch = EBase "Literal"
    (      Text "expected " :<>: Text (ShowChar chNext)
      :<>: Text    ", got " :<>: Text (ShowChar ch))

eWrongChar :: SChar chNext -> SChar ch -> SE (EWrongChar chNext ch)
eWrongChar chNext ch = SEBase symbolSing $
          SText symbolSing :$<>: SText (sShowChar chNext)
    :$<>: SText symbolSing :$<>: SText (sShowChar ch)

type LiteralChSym :: ParserChSym Symbol ()
data LiteralChSym f
type instance App LiteralChSym f = LiteralChSym1 f

type LiteralChSym1 :: ParserChSym1 Symbol ()
data LiteralChSym1 ch s
type instance App (LiteralChSym1 ch) s = LiteralCh ch s

sLiteralChSym :: SParserChSym SSymbol SUnit LiteralChSym
sLiteralChSym = Lam2 $ \ch str ->
    case sUnconsSymbol str of
      SJust (STuple2 chNext str') ->
        case testEquality ch chNext of
          Just Refl -> SCont str'
          Nothing   -> unsafeCoerce $ SErr $ eWrongChar chNext ch
      SNothing -> SDone SUnit

type LiteralEnd :: PParserEnd Symbol ()
type family LiteralEnd str where
    LiteralEnd ""  = Right '()
    LiteralEnd str = Left (EStillParsing str)

type EStillParsing str =
    EBase "Literal" (Text "still parsing literal: " :<>: Text str)

eStillParsing :: SSymbol str -> SE (EStillParsing str)
eStillParsing str = withKnownSymbol str singE

type LiteralEndSym :: ParserEndSym Symbol ()
data LiteralEndSym s
type instance App LiteralEndSym s = LiteralEnd s

sLiteralEndSym :: SParserEndSym SSymbol SUnit LiteralEndSym
sLiteralEndSym = Lam $ \str ->
    case testEquality str (SSymbol @"") of
      Just Refl -> SRight SUnit
      Nothing   -> unsafeCoerce $ SLeft $ eStillParsing str
