{-# LANGUAGE AllowAmbiguousTypes #-}

{- TODO
* we can't reify non-consuming checks because it means our parser is now a type
  synonym family instead of a type synonym. so we might only be able to reify
  unsafe versions
* we can't reify ANY parsers that use type families in the top-level. shit.
  we may have to write yet another layer that hides top-level behind a defun (?)
  symbol
-}

{- NEW TODO s
* would be nice to report an Int in ERun instead. needs to have a type mode and
  a term mode. low prio though
* ack actually yeah, E stores an ErrorMessage which we can't have term level heh
* in an indeal world we'd write our own failure type which has type- and term-
  modes. will take a while though since it's boring lol
-}

{- NOTES
* oops, we don't need to ferry custom singleton constraints around, not even for
  internal GHC singletons. They always have a
  @withKnownX :: forall (x :: X). SX x -> (KnownX x => r) -> r@.
-}

module Symparsec.Reify where

import Symparsec.Parser
import Data.Kind ( Type, Constraint )
import GHC.Exts ( Proxy#, proxy# )

import Symparsec.Parser.Literal

import GHC.TypeLits
import Symparsec.Run

runz :: Z p => String -> ()
runz = \case
  []     -> ()
  ch:chs -> ()

class Z (p :: Parser s r) where
    type PSDemote p :: Type
    type PSSing   p :: Type
    pSDemote :: PSSing p -> PSDemote p
instance Z (Literal'' ch msym) where
    type PSDemote (Literal'' ch msym) = (Char, String)
    type PSSing   (Literal'' ch msym) = (SChar ch, SSymbol (ReconsSymbol msym))
    pSDemote ((sch :: SChar c), (ssym :: SSymbol sym)) =
        withKnownChar sch $ withKnownSymbol ssym $
            (charVal' (proxy# @c), symbolVal' (proxy# @sym))

--zs :: forall p. Z p => PSDemote p
--zs = pSDemote @(PS p)

type PS (p :: Parser s r) = s
{-
--class XYZ tag where
type ParserS :: p -> s
type family ParserS p
class XYZ' tag where
    -- separate type class due to recursive group issue
    type Demote tag :: Type
    withSomeSing
        :: Demote tag
        -> (forall (a :: ParserS tag). ParserS a -> r)
        -> r

type instance ParserS (ch :: Char) = SChar ch
instance XYZ' Char where
    --type SingC  Char ch = KnownChar ch
    type Demote Char    = Char
    withSomeSing ch f = withSomeSChar ch $ \(sch :: SChar ch) ->
        withKnownChar sch $ f
        f sch
-}

{-
class XYZ k where
    type Demote k :: Type
    type Sing   k :: Type
    type SingKind XYZReifyC k (a :: k) :: Constraint
    withSomeSing
        :: Demote k
        -> (forall (a :: k). Sing a -> r)
        -> r
-}

{-
instance XYZ Char where
    type XYZReify  Char = Char
    type XYZReifyC Char ch = KnownChar ch
    withXyz ch f = withSomeSChar ch $ \(_ :: SChar ch) -> f (proxy# @ch)
-}

-- essentially a little singletons definition here
class X p where
    type XS p :: s
    type XReifyS p :: Type
    type XReifySC p :: Constraint
    withReflectS
        :: forall r
        .  XReifySC p
        => XReifyS p
        -- -> (forall (s :: XS p). Proxy# s -> r)
        -- ^ TODO can't do the above because "expected a type" where @XS p@.
        --   why?
        -- -> (Proxy# (XS p) -> r)
        -> r
instance X (Literal'' ch msym) where
    type XS       (Literal'' ch msym) = (Char, Maybe (Char, Symbol))
    type XReifyS  (Literal'' ch msym) = (Char, String)
    type XReifySC (Literal'' ch msym) =
        (KnownChar ch, KnownSymbol (ReconsSymbol msym))
{-
    withReflectS (ch, sym) f = withSomeSChar ch $ \(sch :: SChar ch') ->
        withSomeSSymbol sym $ \(ssym :: SSymbol sym') ->
            f (proxy# @'(ch', UnconsSymbol sym'))
            --f (proxy# @(Char, Maybe (Char, Symbol)))
-}

--instance XParser (Literal'' ch msym) where
--    type XParserS (Literal'' ch msym) = (Char, Maybe (Char, Symbol))

run
    :: forall {s} {r} (p :: Parser s r)
    .  (Reify p, ReifySC p)
    => String
    -> Either ERun (ReifyR p, String)
run = \case
  []     ->
    case pEnd @p s of
      Right r -> Right (r, "")
      Left  e -> Left  $ ERun0 e
  ch:chs -> runCh @p 0 ch chs (pCh @p ch s)
  where
    s = sInit @p

runCh
    :: forall {s} {r} (p :: Parser s r)
    .  Reify p
    => Natural
    -> Char
    -> String
    -> Result (ReifyS p) (ReifyR p)
    -> Either ERun (ReifyR p, String)
runCh idx ch chs = \case
  Cont s ->
    case chs of
      [] ->
        case pEnd @p s of
          Right r -> Right (r, "")
          Left  e -> Left  $ ERun idx ch e
      ch':chs' ->
        runCh @p (idx+1) ch' chs' (pCh @p ch' s)
  Done r -> Right (r, chs)
  Err  e -> Left  $ ERun idx ch e

class Reify (p :: Parser s r) where
    type ReifyR p :: Type
    type ReifyS p :: Type
    type ReifySC p :: Constraint
    sInit' :: ReifySC p => ReifyS p
    pEnd'  :: ReifyS p -> Either E (ReifyR p)
    pCh'   :: Char -> ReifyS p -> Result (ReifyS p) (ReifyR p)

sInit
    :: forall {s} {r} (p :: Parser s r)
    .  (Reify p, ReifySC p) => ReifyS p
sInit = sInit' @_ @_ @p

pEnd
    :: forall {s} {r} (p :: Parser s r)
    .  Reify p => ReifyS p -> Either E (ReifyR p)
pEnd = pEnd' @_ @_ @p

pCh
    :: forall {s} {r} (p :: Parser s r)
    .  Reify p => Char -> ReifyS p -> Result (ReifyS p) (ReifyR p)
pCh = pCh' @_ @_ @p

instance Reify (Literal'' ch msym) where
    type ReifyR (Literal'' ch msym) = ()
    type ReifyS (Literal'' ch msym) = (Char, String)
    type ReifySC (Literal'' ch msym) =
        (KnownChar ch, KnownSymbol (ReconsSymbol msym))
    sInit' = (charVal'' @ch, symbolVal'' @(ReconsSymbol msym))
    pEnd'  (_ch, _sym) = Left undefined
    pCh'   ch (ch', sym) =
        if ch == ch'
        then case sym of
               []       -> Done ()
               ch'':chs -> Cont (ch'', chs)
        else Err undefined

charVal'' :: forall ch. KnownChar ch => Char
charVal'' = charVal' (proxy# @ch)

symbolVal'' :: forall sym. KnownSymbol sym => String
symbolVal'' = symbolVal' (proxy# @sym)

{-

class Reify (m :: Type -> Type) (p :: Parser s r) where
    type ReifyResult m p :: Type
    reify :: m (ReifyResult m p)

instance (ReifyTake n m, KnownNat n) => Reify m (Take' n) where
    type ReifyResult m (Take' n) = Chunk m
    reify = reifyTake @n (natValInt @n)

instance (ReifyIsolate m n '(pCh, pEnd, s), KnownNat n)
  => Reify m (Isolate'' n pCh pEnd s) where
    type ReifyResult m (Isolate'' n pCh pEnd s) =
        ReifyResult m '(pCh, pEnd, s)
    reify = reifyIsolate @_ @_ @m @n @'(pCh, pEnd, s) (natValInt @n)

-- can't define this in Isolate because of recursive constraint LOL
class Reify m p => ReifyIsolate m (n :: Natural) (p :: Parser s r) where
    reifyIsolate :: Int -> m (ReifyResult m p)

---

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal' (proxy# @n)

-}
