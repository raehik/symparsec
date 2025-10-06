{-# LANGUAGE UndecidableInstances #-}

module Symparsec.Parser.Or where

import Symparsec.Parser
import TypeLevelShow.Doc
import GHC.TypeLits hiding ( ErrorMessage(..) )
import DeFun.Core
import Singleraeh.Tuple
import Singleraeh.Either
import Singleraeh.List

{- | Limited parser choice. Try left; if it fails, backtrack and try right.
     However, _the right choice must consume at least as much as the left
     choice._ If it doesn't, then even if the right parser succeeds, it
     will emit an error.

This behaviour is due to the parser runner not supporting backtracking. We can
emulate it by storing a record of the characters parsed so far, and "replaying"
these on the right parser if the left parser fails. If the right parser ends
before we finish replaying, we will have consumed extra characters that we can't
ask the runner to revert.

For example, @Literal "abcd" :<|>: Literal "ab"@ is bad. An input of @abcX@ will
trigger the consumption error.

I can't think of another way to implement this with the current parser design. I
think it's the best we have. A more complex parser design may permit changing
internal running state, so we could save and load state (this would permit a
@Try p@ parser). But that's scary. And you're better off designing your
type-level string schemas to permit non-backtracking parsing anyway...

Also problematic is that we never emit a left parser error, so errors can
degrade. Perhaps your string was one character off a successful left parse; but
if it fails, you won't see that error.
-}
infixl 3 :<|>:
type (:<|>:)
    :: PParser sl rl
    -> PParser sr rr
    -> PParser (OrS sl sr) (Either rl rr)
type family pl :<|>: pr where
    'PParser plCh plEnd s0l :<|>: 'PParser prCh prEnd s0r =
        Or' plCh plEnd s0l prCh prEnd s0r

type Or' plCh plEnd s0l prCh prEnd s0r = 'PParser
    (OrChSym plCh prCh s0r)
    (OrEndSym plEnd prCh prEnd s0r)
    (Left '(s0l, '[]))

type SOrS ssl ssr = SEither (STuple2 ssl (SList SChar)) ssr
type OrS  sl  sr  = Either (sl, [Char]) sr
type SPOr ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r =
    SParser (SOrS ssl ssr) (SEither srl srr) (Or' plCh plEnd s0l prCh prEnd s0r)

sOr
    :: SParser ssl srl ('PParser plCh plEnd s0l)
    -> SParser ssr srr ('PParser prCh prEnd s0r)
    -> SPOr    ssl srl ssr srr plCh plEnd s0l prCh prEnd s0r
sOr (SParser plCh plEnd s0l) (SParser prCh prEnd s0r) = SParser
    (sOrChSym plCh prCh s0r)
    (sOrEndSym plEnd prCh prEnd s0r)
    (SLeft (STuple2 s0l SNil))

instance
  ( pl ~ 'PParser plCh plEnd s0l
  , pr ~ 'PParser prCh prEnd s0r
  , SingParser pl
  , SingParser pr
  ) => SingParser (Or' plCh plEnd s0l prCh prEnd s0r) where
    type PS (Or' plCh plEnd s0l prCh prEnd s0r) = SOrS
        (PS ('PParser plCh plEnd s0l))
        (PS ('PParser prCh prEnd s0r))
    type PR (Or' plCh plEnd s0l prCh prEnd s0r) = SEither
        (PR ('PParser plCh plEnd s0l))
        (PR ('PParser prCh prEnd s0r))
    singParser' = sOr (singParser @pl) (singParser @pr)

type OrCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> PParserCh (OrS sl sr) (Either rl rr)
type family OrCh plCh prCh sr ch s where
    -- | parsing left
    OrCh plCh prCh s0r ch (Left  '(sl, chs)) =
        OrChL prCh s0r ch chs (plCh @@ ch @@ sl)

    -- | parsing right (after left failed and was successfully replayed)
    OrCh plCh prCh _  ch (Right sr) =
        OrChR (prCh @@ ch @@ sr)

type OrChL
    :: ParserChSym sr rr
    -> sr
    -> Char
    -> [Char]
    -> PResult sl rl
    -> PResult (Either (sl, [Char]) sr) (Either rl rr)
type family OrChL prCh s0r chLast chs resl where
    -- | left parser OK, continue
    OrChL _    _   chLast chs (Cont sl) = Cont (Left  '(sl, chLast : chs))

    -- | left parser OK, done
    OrChL _    _   _      _   (Done rl) = Done (Left  rl)

    -- | left parser failed: ignore, replay consumed characters on right parser
    OrChL prCh s0r chLast chs (Err  _ ) =
        OrChLReplay prCh chLast (Reverse chs) (Cont s0r)

type OrChLReplay
    :: ParserChSym sr rr
    -> Char
    -> [Char]
    -> PResult sr rr
    -> PResult (Either (sl, [Char]) sr) (Either rl rr)
type family OrChLReplay prCh chLast chs resr where
    -- | right parser OK, keep replaying
    OrChLReplay prCh chLast (ch : chs) (Cont sr) =
        OrChLReplay prCh chLast chs (prCh @@ ch @@ sr)
    --
    -- | right parser OK, final replay char
    OrChLReplay prCh chLast '[]        (Cont sr) = OrChR (prCh @@ chLast @@ sr)

    -- | right parser fail: wrap error
    OrChLReplay prCh chLast chs        (Err  er) = Err (EOrR er)

    -- | right parser done but still replaying! no choice but to error out
    OrChLReplay prCh chLast chs        (Done rr) = Err EOrStillReplaying
        -- Done (Right rr) -- TODO

sOrChLReplay
    :: SParserChSym ssr srr prCh
    -> SChar chLast
    -> SList SChar chs
    -> SResult ssr srr resr
    -> SResult (SOrS ssl ssr) (SEither srl srr)
        (OrChLReplay prCh chLast chs resr)
sOrChLReplay prCh chLast chs resr =
    case chs of
      SCons ch chs' ->
        case resr of
          SCont  sr ->
            sOrChLReplay prCh chLast chs' (prCh @@ ch @@ sr)
          SDone _rr -> SErr eOrStillReplaying
          SErr   er -> SErr $ eOrR er
      SNil ->
        case resr of
          SCont  sr -> sOrChR (prCh @@ chLast @@ sr)
          SDone _rr -> SErr eOrStillReplaying
          SErr   er -> SErr $ eOrR er

type EOrR er = EIn "Or(R)" er
eOrR :: SE er -> SE (EOrR er)
eOrR er = SEIn symbolSing er

type EOrStillReplaying = EBase "Or"
    (Text "right parser much consume at least as much as the failed left parser")
eOrStillReplaying :: SE EOrStillReplaying
eOrStillReplaying = singE

type family OrChR resr where
    OrChR (Cont sr) = Cont (Right sr)
    OrChR (Done rr) = Done (Right rr)
    OrChR (Err  er) = Err (EOrR er)

sOrChR
    :: SResult ssr srr resr
    -> SResult (SOrS ssl ssr) (SEither srl srr) (OrChR resr)
sOrChR = \case
  SCont sr -> SCont $ SRight sr
  SDone rr -> SDone $ SRight rr
  SErr  er -> SErr  $ eOrR er

type OrEnd
    :: ParserEndSym sl rl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> PParserEnd (Either (sl, [Char]) sr) (Either rl rr)
type family OrEnd plEnd prCh prEnd sr res where
    -- | input ended during left parser
    OrEnd plEnd prCh prEnd s0r (Left  '(sl, chs)) =
        OrEndL prCh prEnd s0r chs (plEnd @@ sl)

    -- | input ended during right parser: call right end
    OrEnd plEnd prCh prEnd _   (Right sr)         = OrEndR (prEnd @@ sr)

type OrEndR :: PResultEnd rr -> PResultEnd (Either rl rr)
type family OrEndR resr where
    OrEndR (Right rr) = Right (Right rr)
    OrEndR (Left  er) = Left  (EOrR er)

sOrEndR
    :: SResultEnd srr resr
    -> SResultEnd (SEither srl srr) (OrEndR resr)
sOrEndR = \case
  SRight rr -> SRight $ SRight rr
  SLeft  er -> SLeft  $ eOrR er

type OrEndL
    :: ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> [Char]
    -> Either PE rl
    -> Either PE (Either rl rr)
type family OrEndL prCh prEnd s0r chs resl where
    -- | input ended during left parser and left end succeeeded: phew
    OrEndL prCh prEnd s0r chs (Right rl) = Right (Left rl)

    -- | input ended during left parser and left end failed. replay on right,
    --   then eventually call right end
    OrEndL prCh prEnd s0r chs (Left  el) =
        OrEndLReplay prCh prEnd (Reverse chs) (Cont s0r)

sOrEndL
    :: SParserChSym  ssr srr prCh
    -> SParserEndSym ssr srr prEnd
    -> ssr s0r
    -> SList SChar chs
    -> SResultEnd srl resl
    -> SResultEnd (SEither srl srr) (OrEndL prCh prEnd s0r chs resl)
sOrEndL prCh prEnd s0r chs = \case
  SRight  rl -> SRight $ SLeft rl
  SLeft  _el -> sOrEndLReplay prCh prEnd (sReverse chs) (SCont s0r)

type OrEndLReplay
    :: ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> [Char]
    -> PResult sr rr
    -> Either PE (Either rl rr)
type family OrEndLReplay prCh prEnd chs resr where
    -- | right parser OK, keep replaying
    OrEndLReplay prCh prEnd (ch : chs) (Cont sr) =
        OrEndLReplay prCh prEnd chs (prCh @@ ch @@ sr)

    -- | replay complete
    OrEndLReplay prCh prEnd '[]        resr      = OrEndLReplay' prEnd resr

    -- | right parser fail: wrap error
    OrEndLReplay prCh prEnd chs        (Err  er) = Left (EOrR er)

    -- | right parser done but still replaying! no choice but to error out
    OrEndLReplay prCh prEnd chs        (Done rr) = Left EOrStillReplaying
        -- Right (Right rr) TODO

sOrEndLReplay
    :: SParserChSym  ssr srr prCh
    -> SParserEndSym ssr srr prEnd
    -> SList SChar chs
    -> SResult ssr srr resr
    -> SResultEnd (SEither srl srr) (OrEndLReplay prCh prEnd chs resr)
sOrEndLReplay prCh prEnd chs resr =
    case chs of
      SCons ch chs' ->
        case resr of
          SCont  sr ->
            sOrEndLReplay prCh prEnd chs' (prCh @@ ch @@ sr)
          SErr   er -> SLeft $ eOrR er
          SDone _rr -> SLeft eOrStillReplaying
      SNil ->
        case resr of
          SCont sr -> sOrEndR $ prEnd @@ sr
          SDone rr -> SRight  $ SRight rr
          SErr  er -> SLeft   $ eOrR er

type family OrEndLReplay' prEnd resr where
    OrEndLReplay' prEnd (Cont sr) = OrEndR (prEnd @@ sr)
    OrEndLReplay' prEnd (Done rr) = Right  (Right rr)
    OrEndLReplay' prEnd (Err  er) = Left   (EOrR er)

type OrChSym
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym (OrS sl sr) (Either rl rr)
data OrChSym plCh prCh s0r f
type instance App (OrChSym plCh prCh s0r) f = OrChSym1 plCh prCh s0r f

type OrChSym1
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserChSym1 (OrS sl sr) (Either rl rr)
data OrChSym1 plCh prCh sr ch s
type instance App (OrChSym1 plCh prCh sr ch) s = OrCh plCh prCh sr ch s

sOrChSym
    :: SParserChSym ssl srl plCh
    -> SParserChSym ssr srr prCh
    -> ssr s0r
    -> SParserChSym (SOrS ssl ssr) (SEither srl srr) (OrChSym plCh prCh s0r)
sOrChSym plCh prCh s0r = Lam2 $ \ch -> \case
  SLeft  (STuple2 sl chs) -> sOrChL prCh s0r ch chs (plCh @@ ch @@ sl)
  SRight sr -> sOrChR (prCh @@ ch @@ sr)

sOrChL
    :: SParserChSym ssr srr prCh
    -> ssr s0r
    -> SChar chLast
    -> SList SChar chs
    -> SResult ssl srl resl
    -> SResult (SOrS ssl ssr) (SEither srl srr) (OrChL prCh s0r chLast chs resl)
sOrChL prCh s0r chLast chs = \case
  SCont  sl -> SCont $ SLeft $ STuple2 sl $ SCons chLast chs
  SDone  rl -> SDone $ SLeft rl
  SErr  _el -> sOrChLReplay prCh chLast (sReverse chs) (SCont s0r)

type OrEndSym
    :: ParserEndSym sl rl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> ParserEndSym (Either (sl, [Char]) sr) (Either rl rr)
data OrEndSym plEnd prCh prEnd s0r s
type instance App (OrEndSym plEnd prCh prEnd s0r) s =
    OrEnd plEnd prCh prEnd s0r s

sOrEndSym
    :: SParserEndSym ssl srl plEnd
    -> SParserChSym  ssr srr prCh
    -> SParserEndSym ssr srr prEnd
    -> ssr s0r
    -> SParserEndSym (SOrS ssl ssr) (SEither srl srr)
        (OrEndSym plEnd prCh prEnd s0r)
sOrEndSym plEnd prCh prEnd s0r = Lam $ \case
  SLeft (STuple2 sl chs) -> sOrEndL prCh prEnd s0r chs (plEnd @@ sl)
  SRight sr -> sOrEndR $ prEnd @@ sr
