-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Tmp where

import GHC.TypeNats
import GHC.TypeLits ( Symbol, UnconsSymbol, ConsSymbol, ErrorMessage )
import Data.Kind ( Type )
import DeFun.Core ( type (~>), type App )

-- | Parser error.
data E
  -- | Base parser error.
  = EBase
        Symbol       -- ^ parser name
        ErrorMessage -- ^ error message

  -- | Inner parser error inside combinator.
  | EIn
        Symbol -- ^ combinator name
        E      -- ^ inner error

-- | The result of a single step of a parser.
data Result s r
  = Cont s -- ^ OK, continue with the given state
  | Done r -- ^ OK, parse successful with result @r@
  | Err  E -- ^ parse error

-- just a nicer 3-tuple :)
data Parser s r = Parser
  { parserCh   :: ParserChSym  s r
  , parserEnd  :: ParserEndSym s r
  , parserInit :: s
  }

type ParserChSym  s r = Char ~> s ~> Result s r
type ParserChSym1 s r = Char -> s ~> Result s r
type ParserEndSym s r = s ~> Either E r

{-

type ParserChSym :: s -> r -> Char ~> s ~> Result s r
data ParserChSym s r f -- = ParserChSym (Char ~> s ~> Result s r)
type instance App (ParserChSym s r) f = ParserChSym1 s r f

type ParserChSym1 :: s -> r -> Char -> s ~> Result s r
data ParserChSym1 s r ch s' -- = ParserChSym (Char ~> s ~> Result s r)
type instance App (ParserChSym1 s r ch) s' = Err ()

-- | Re-construct the output from 'UnconsSymbol'.
type family ReconsSymbol msym where
    ReconsSymbol Nothing           = ""
    ReconsSymbol (Just '(ch, sym)) = ConsSymbol ch sym

data Result s r = Cont s | Done r | Err ()

-- | must be separate to Parser or "same recursive group" error
class ParserTypes p where
    -- | Parser state.
    type PState p :: Type

    -- | Parser return type.
    type PReturn p :: Type

class Parser p where
    -- | Initial parser state.
    type PState0 p :: PState p

    -- | Character parser.
    type PCh p (ch :: Char) (s :: PState p) :: Result (PState p) (PReturn p)

    type PEnd p (s :: PState p) :: Either () (PReturn p)

{- Ack. Now we can't write Run, because we can't access state or return types.
   I think I do need the tuple method (or a custom tuple type, whatever).
   I need to be able to do algebra on parser types, not have it hidden behind
   instances and type families. (Same reason I need the defun symbols, to a
   degree.) Great investigation regardless!
type Run :: (p :: Type) -> Symbol -> Either () (PReturn p)
type Run p sym = RunStart p (PState0 p) (UnconsSymbol sym)

type family RunStart p s msym where
    RunStart p s Nothing           = Left '()
    RunStart p s (Just '(ch, sym)) =
        RunCh p 0 ch (UnconsSymbol sym) (PCh p ch s)

type RunCh
    :: (p :: Type)
    -> Natural -> Char -> Maybe (Char, Symbol)
    -> Result (PState p) (PReturn p)
    -> Either () (PReturn p, Symbol)
type family RunCh p idx ch' msym res where
    RunCh p idx ch' (Just '(ch, sym)) (Cont s) =
        --RunCh p idx ch (UnconsSymbol sym) (PCh p ch s)
        Left '()
    RunCh p idx ch' Nothing           (Cont s) =
        --RunEnd p idx ch' (PEnd p s)
        Left '()
    RunCh p idx ch' msym              (Done r) =
        Right '(r, ReconsSymbol msym)
    RunCh p idx ch' msym              (Err  e) =
        Left '()

type family RunEnd p idx ch res where
    RunEnd p idx ch (Right r) = Right '(r, "")
    RunEnd p idx ch (Left  e) = Left '()
-}

class Reify p where
    -- | Parser state singleton type.
    type PStateS p :: PState p -> Type

    -- | Parser state demoted type.
    --type PStateD p (s :: PState p) = (r :: Type) | r -> s
    --type PStateD p (s :: PState p) :: Type
    type PStateD p :: Type

    fromPStateS :: PStateS p s -> PStateD p
    toPStateS   :: PStateD p -> SomePStateS p

data SomePStateS p where
    SomePStateS :: PStateS p s -> SomePStateS p

data Skip (n :: Natural)
instance ParserTypes (Skip n) where
    type PState  (Skip n) = Natural
    type PReturn (Skip n) = ()
$(return []) -- due to GHC bug (see wip/foldmapsumtype)
instance Parser (Skip n) where
    type PState0 (Skip n)     = n
    type PCh     (Skip n) _ s = SkipCh s
    type PEnd    (Skip n)   s = SkipEnd s
instance Reify (Skip n) where
    type PStateS (Skip n) = SNat
    type PStateD (Skip n) = Natural
    fromPStateS = fromSNat
    toPStateS n = withSomeSNat n SomePStateS

type family SkipCh n where
    SkipCh 1 = Done '()
    SkipCh n = Cont (n-1)

type family SkipEnd n where
    SkipEnd 0 = Right '()
    SkipEnd n = Left '()

{- ayy this works chief! but I want to try rejigging the design...
class Run (p :: Parser s r) where
    type RunDemoteS s = (sd :: Type) | sd -> s
    type RunSingS :: s -> Type
    runFromSingS :: RunSingS (sa :: s) -> RunDemoteS s
    runToSingS   :: RunDemoteS s -> SomeRunSingS s

data SomeRunSingS s where
    SomeRunSingS :: RunSingS (sa :: s) -> SomeRunSingS s

instance Run (Skip' n) where
    type RunDemoteS Natural = Natural
    type RunSingS = SNat
    runFromSingS = fromSNat
    runToSingS n = withSomeSNat n SomeRunSingS
-}

{-

type instance Sing = SNat
instance SingKind Natural where
    type Demote Natural = Natural
    fromSing = fromSNat
    toSing n = withSomeSNat n SomeSing

type instance Sing = (,)
instance SingKind (a, b) where
    type Demote (a, b) = (a, b)
    --fromSing = fromSNat
    --toSing n = withSomeSNat n SomeSing

class Run (p :: Parser s r) where
    type PSDemote p :: Type
    type PSSing   p :: Type
    pSDemote :: PSSing p -> PSDemote p
instance Z (Literal'' ch msym) where
    type PSDemote (Literal'' ch msym) = (Char, String)
    type PSSing   (Literal'' ch msym) = (SChar ch, SSymbol (ReconsSymbol msym))
    pSDemote ((sch :: SChar c), (ssym :: SSymbol sym)) =
        withKnownChar sch $ withKnownSymbol ssym $
            (charVal' (proxy# @c), symbolVal' (proxy# @sym))

type PS (p :: Parser s r) = s

run :: Run p => String -> ()
run = \case
  []     -> ()
  ch:chs -> ()

-}

-}
