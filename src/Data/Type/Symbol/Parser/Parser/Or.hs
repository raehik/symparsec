{-# LANGUAGE UndecidableInstances #-}

{- | Choice operator. Try left; if it fails, try right.

This is a problematic combinator:

  * Implementation is clumsy due to internal parser state being untouchable.
    We must record seen characters, in order to replay them on the right parser
    in case the left parser fails.
  * Errors degrade due to left parser errors being discarded. Perhaps your
    string was one character off a successful left parse; but if it fails, you
    won't see that error.
  * It's hard to reason aobut. It might break in certain situations.
-}


module Data.Type.Symbol.Parser.Parser.Or ( Or ) where

import Data.Type.Symbol.Parser.Types
import DeFun.Core ( type (@@), type App )
import Data.Type.List ( Reverse )

type Or
    :: Parser sl rl
    -> Parser sr rr
    -> Parser (Either (sl, [Char]) sr) (Either rl rr)
type family Or pl pr where
    Or '(plCh, plEnd, sl) '(prCh, prEnd, sr) =
        '(OrChSym plCh prCh sr, OrEndSym plEnd prCh prEnd sr, Left '(sl, '[]))

type OrCh
    :: ParserChSym sl rl
    -> ParserChSym sr rr
    -> sr
    -> ParserCh (Either (sl, [Char]) sr) (Either rl rr)
type family OrCh plCh prCh sr ch s where
    -- | Parsing left
    OrCh plCh prCh sr ch (Left  '(sl, chs)) =
        OrChL prCh sr (ch : chs) (plCh @@ ch @@ sl)

    -- | Parsing right (left failed and was successfully replayed)
    OrCh plCh prCh _  ch (Right sr) =
        OrChR (prCh @@ ch @@ sr)

-- TODO [Char] is actually nonempty, but it's awkward because we need to reverse
-- it and place the known char at the end. we _can_ fix this, but it's just
-- annoying lol
type OrChL
    :: ParserChSym sr rr
    -> sr
    -> [Char]
    -> Result sl rl
    -> Result (Either (sl, [Char]) sr) (Either rl rr)
type family OrChL prCh sr chs resl where
    -- | Left parser OK, continue
    OrChL _    _  chs (Cont sl) = Cont (Left  '(sl, chs))

    -- | Left parser OK, done
    OrChL _    _  chs (Done rl) = Done (Left  rl)

    -- | Left parser failed: ignore, replay consumed characters on right parser
    OrChL prCh sr chs (Err  _ ) =
        OrChLReplay prCh (Reverse chs) (Cont sr)

-- TODO [Char] is nonempty, see 'OrChL'. also if we fix that, we can get better
-- equation ordering!
type OrChLReplay
    :: ParserChSym sr rr
    -> [Char]
    -> Result sr rr
    -> Result (Either (sl, [Char]) sr) (Either rl rr)
type family OrChLReplay prCh chs resr where
    -- | Right parser OK, last char
    OrChLReplay prCh (ch : '[]) (Cont sr) = OrChR (prCh @@ ch @@ sr)

    -- | Right parser OK, keep replaying
    OrChLReplay prCh (ch : chs) (Cont sr) =
        OrChLReplay prCh chs (prCh @@ ch @@ sr)

    -- | Right parser fail: wrap error
    --
    -- TODO error behaviour here?
    OrChLReplay prCh chs        (Err  er) = Err (EIn "Or(R)" er)

    -- | Right parser done: early success
    --
    -- If this matches before we finish replaying, any remaining replay
    -- characters are lost. This _should_ break certain parses. Or, maybe it
    -- only breaks return symbol..? In which case, we can fix by returning
    -- the extra replayed chars in our return type???? TODO
    OrChLReplay prCh chs        (Done rr) = Done (Right rr)
        -- (EBase "Or" (ErrParserLimitation "cannot parse less on right of Or"))

type family OrChR resr where
    OrChR (Cont sr) = Cont (Right sr)
    OrChR (Done rr) = Done (Right rr)
    OrChR (Err  er) = Err (EIn "Or(R)" er)

type OrEnd
    :: ParserEndSym sl rl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> ParserEnd (Either (sl, [Char]) sr) (Either rl rr)
type family OrEnd plEnd prCh prEnd sr res where
    -- | Input ended on L.
    OrEnd plEnd prCh prEnd sr (Left  '(sl, chs)) =
        OrEndL prCh prEnd sr chs (plEnd @@ sl)

    -- | Input ended on R.
    OrEnd plEnd prCh prEnd _  (Right sr)         = OrEndR (prEnd @@ sr)

type OrEndR
    :: Either E rr
    -> Either E (Either rl rr)
type family OrEndR s where
    OrEndR (Left  er) = Left (EIn "Or(R)" er)
    OrEndR (Right rr) = Right (Right rr)

type OrEndL
    :: ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> [Char]
    -> Either E rl
    -> Either E (Either rl rr)
type family OrEndL prCh prEnd sr chs res where
    OrEndL prCh prEnd sr chs (Right rl) = Right (Left rl)
    OrEndL prCh prEnd sr chs (Left  el) =
        OrChLReplay' prCh prEnd (Reverse chs) (Cont sr)

type OrChLReplay'
    :: ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> [Char]
    -> Result sr rr
    -> Either E (Either rl rr)
type family OrChLReplay' prCh prEnd chs resr where
    OrChLReplay' prCh prEnd (ch : '[]) (Cont sr) =
        OrEndR' prEnd (prCh @@ ch @@ sr)
    OrChLReplay' prCh prEnd (ch : chs) (Cont sr) =
        OrChLReplay' prCh prEnd chs (prCh @@ ch @@ sr)
    OrChLReplay' prCh prEnd chs        (Err  er) = Left (EIn "Or(R)" er)

    -- TODO Here, we successfully ended while replaying left-consumed chars.
    -- This may not work if it's wrapped in further combinators, I'm unsure.
    OrChLReplay' prCh prEnd chs        (Done rr) = Right (Right rr)
        -- (EBase "Or" (ErrParserLimitation "cannot parse less on right of Or"))

type family OrEndR' prEnd s where
    OrEndR' prEnd (Err  er) = Left (EIn "Or(R)" er)
    OrEndR' prEnd (Done rr) = Right (Right rr)
    OrEndR' prEnd (Cont sr) = OrEndR (prEnd @@ sr)

data OrChSym plCh prCh sr f
type instance App (OrChSym plCh prCh sr) f = OrChSym1 plCh prCh sr f

data OrChSym1 plCh prCh sr ch s
type instance App (OrChSym1 plCh prCh sr ch) s = OrCh plCh prCh sr ch s

type OrEndSym
    :: ParserEndSym sl rl
    -> ParserChSym  sr rr
    -> ParserEndSym sr rr
    -> sr
    -> ParserEndSym (Either (sl, [Char]) sr) (Either rl rr)
data OrEndSym plEnd prCh prEnd sr s
type instance App (OrEndSym plEnd prCh prEnd sr) s = OrEnd plEnd prCh prEnd sr s
