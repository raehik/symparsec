{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Skip where

import Symparsec.Parser.Common
import GHC.TypeLits hiding ( ErrorMessage(..) )
import TypeLevelShow.Natural ( ShowNatDec, sShowNatDec )
import Data.Type.Equality
import Singleraeh.Tuple
import Singleraeh.Either
import Singleraeh.Natural
import DeFun.Core
import Unsafe.Coerce ( unsafeCoerce )

-- | Skip forward @n@ characters. Fails if fewer than @n@ characters are
--   available'.
type Skip :: Natural -> PParser Natural ()
type Skip n = 'PParser SkipChSym SkipEndSym n

sSkip :: SNat n -> SParser SNat SUnit (Skip n)
sSkip n = SParser sSkipChSym sSkipEndSym n

instance KnownNat n => SingParser (Skip n) where
    type PS (Skip n) = SNat
    type PR (Skip n) = SUnit
    singParser' = sSkip SNat

type SkipCh :: PParserCh Natural ()
type family SkipCh ch n where
    SkipCh _ 0 = Done '()
    SkipCh _ n = Cont (n-1)

type SkipChSym :: ParserChSym Natural ()
data SkipChSym f
type instance App SkipChSym f = SkipChSym1 f

type SkipChSym1 :: ParserChSym1 Natural ()
data SkipChSym1 ch n
type instance App (SkipChSym1 ch) n = SkipCh ch n

sSkipChSym :: SParserChSym SNat SUnit SkipChSym
sSkipChSym = Lam2 $ \_ n ->
    case testEquality n (SNat @0) of
      Just Refl -> SDone SUnit
      Nothing   -> unsafeCoerce $ SCont $ n %- (SNat @1)

type SkipEnd :: PParserEnd Natural ()
type family SkipEnd n where
    SkipEnd 0 = Right '()
    SkipEnd n = Left (ESkipPastEnd n)

type ESkipPastEnd n = EBase "Skip"
    (      Text "tried to drop "
      :<>: Text (ShowNatDec n) :<>: Text " chars from empty symbol")
eSkipPastEnd :: SNat n -> SE (ESkipPastEnd n)
eSkipPastEnd n = withKnownSymbol (sShowNatDec n) singE

type SkipEndSym :: ParserEndSym Natural ()
data SkipEndSym n
type instance App SkipEndSym n = SkipEnd n

sSkipEndSym :: SParserEndSym SNat SUnit SkipEndSym
sSkipEndSym = Lam $ \n ->
    case testEquality n (SNat @0) of
      Just Refl -> SRight SUnit
      Nothing   -> unsafeCoerce $ SLeft $ eSkipPastEnd n
