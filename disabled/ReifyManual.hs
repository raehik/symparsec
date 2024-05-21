{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Type-level parser reification.

Symparsec's design doesn't enable automatic reification of type-level parsers.
However, we may design a parallel term-level parser to make it easy to write
reified versions.

_There are no static guarantees that behaviour matches._ One can be fairly
confident when parsers are simple and closely match their type-level version,
but beware when reifying your own parsers.

I don't consider this an ideal solution, so I define reifications in bulk here
rather than in individual parser modules.
-}

module Symparsec.Reify where

import Symparsec.Parser
import TypeLevelShow.Doc ( Doc(..) )
import Symparsec.Run
import Data.Kind ( Type )

import Symparsec.Parser.Take
import GHC.TypeNats
import GHC.Exts ( proxy# )

import Symparsec.Parser.End
import Symparsec.Parser.Isolate
import Symparsec.Parser.Then

-- | Reify the given type-level parser.
reifyP
    :: forall {s} {r} (p :: ParserSym s r). ReifyP p
    => Parser String (ReifyPS p) (ReifyPR p)
reifyP = Parser (reifyPCh @p) (reifyPEnd @p) (reifyPInit @p)

-- | Run a term-level parser.
run :: Parser String s r -> String -> Either (ERun String) (r, String)
run p = runInit (parserCh p) (parserEnd p) (parserInit p)

runInit
    :: ParserCh  String s r
    -> ParserEnd String s r
    -> s
    -> String
    -> Either (ERun String) (r, String)
runInit pCh pEnd s = \case
  ch:chs -> runCh pCh pEnd 0 ch chs (pCh ch s)
  []     -> runEnd ERun0 (pEnd s)

runEnd0
    :: Either (E String) r
    -> Either (ERun String) (r, String)
runEnd0 = \case
  Right r -> Right (r, "")
  Left  e -> Left  (ERun0 e)

runCh
    :: ParserCh  String s r
    -> ParserEnd String s r
    -> Natural
    -> Char
    -> String
    -> Result    String s r
    -> Either (ERun String) (r, String)
runCh pCh pEnd idx chPrev str = \case
  Cont s ->
    case str of
      ch:chs -> runCh pCh pEnd (idx+1) ch chs (pCh ch s)
      []     -> runEnd (ERun idx chPrev) (pEnd s)
  Done r -> Right (r, str)
  Err  e -> Left  (ERun idx chPrev e)

runEnd
    :: (E String -> ERun String)
    -> Either (E String) r
    -> Either (ERun String) (r, String)
runEnd f = \case
  Right r -> Right (r, "")
  Left  e -> Left  (f e)

{- | Manual reification of type-level parsers.

Assorted notes:

  * No static guarantees that behaviour matches.
  * We reify via the parser type, rather than the internal types. I'm not sure
    this is the right design. It means we can write all definitions in a single
    class and not have to worry about ambiguity (like we would if we used
    @Demote s@, @Demote r@), but it's a little weird and is another obstacle in
    reifying parsers e.g. we can't reify 'Result's because we can't reify @s@
    and @r@ without the parser that use them.
-}
class ReifyP (p :: ParserSym s r) where
    -- | Reified parser state.
    type ReifyPS p :: Type

    -- | Reified parser return type.
    type ReifyPR p :: Type

    -- | Reified character parser.
    reifyPCh'   :: ParserCh  String (ReifyPS p) (ReifyPR p)

    -- | Reified end handler.
    reifyPEnd'  :: ParserEnd String (ReifyPS p) (ReifyPR p)

    -- | Reified initial state.
    reifyPInit' :: ReifyPS p

reifyPCh
    :: forall {s} {r} (p :: ParserSym s r). ReifyP p
    => ParserCh String (ReifyPS p) (ReifyPR p)
reifyPCh = reifyPCh' @_ @_ @p

reifyPEnd
    :: forall {s} {r} (p :: ParserSym s r). ReifyP p
    => ParserEnd String (ReifyPS p) (ReifyPR p)
reifyPEnd = reifyPEnd' @_ @_ @p

reifyPInit
    :: forall {s} {r} (p :: ParserSym s r). ReifyP p
    => ReifyPS p
reifyPInit = reifyPInit' @_ @_ @p

--------------------------------------------------------------------------------

-- | Fail with the given message when given any character to parse.
failCh :: String -> Doc String -> ParserCh String s r
failCh name e _ch _s = Err (EBase name e)

-- | Fail with the given message if we're at the end of the symbol.
failEnd :: String -> Doc String -> ParserEnd String s r
failEnd name e _s = Left (EBase name e)

--------------------------------------------------------------------------------

instance KnownNat n => ReifyP (Take' n) where
    type ReifyPS (Take' n) = (Natural, String)
    type ReifyPR (Take' n) = String
    reifyPCh' ch (n, chs) =
        case n of
          1 -> Done (reverse (ch:chs))
          _ -> Cont (n-1, ch:chs)
    reifyPEnd' (n, chs) =
        case n of
          0 -> Right (reverse chs)
          _ -> Left (EBase "Take"
            (      Text "tried to take "
              :<>: Text (show n) :<>: Text " chars from empty string"))
    reifyPInit' = (natVal' (proxy# @n), [])

instance (ReifyP ('ParserSym plCh plEnd sl), ReifyP ('ParserSym prCh prEnd sr))
  => ReifyP (Then' plCh plEnd sl prCh prEnd sr) where
    -- issue because Then doesn't use plEnd. so we can't bind it. wtf?
    -- fixed by just throwing plEnd into ThenEndSym. unused. stupid
    type ReifyPS (Then' plCh plEnd sl prCh prEnd sr) =
        Either
            (ReifyPS ('ParserSym plCh plEnd sl))
            (ReifyPR ('ParserSym plCh plEnd sl), ReifyPS ('ParserSym prCh prEnd sr))
    type ReifyPR (Then' plCh plEnd sl prCh prEnd sr) =
        (ReifyPR ('ParserSym plCh plEnd sl), ReifyPR ('ParserSym prCh prEnd sr))
    reifyPCh' ch = \case
      Left  sl       ->
        case reifyPCh @('ParserSym plCh plEnd sl) ch sl of
          Cont sl' -> Cont (Left sl')
          Done rl  -> Cont (Right (rl, reifyPInit @('ParserSym prCh prEnd sr)))
          Err  el  -> Err (EIn "Then(L)" el)
      Right (rl, sr) ->
        case reifyPCh @('ParserSym prCh prEnd sr) ch sr of
          Cont sr' -> Cont (Right (rl, sr'))
          Done rr  -> Done (rl, rr)
          Err  er  -> Err (EIn "Then(R)" er)
    reifyPEnd' = \case
      Right (rl, sr) ->
        case reifyPEnd @('ParserSym prCh prEnd sr) sr of
          Right rr -> Right (rl, rr)
          Left  er -> Left (EIn "Then(R)" er)
      Left _sl       -> Left (EBase "Then" (Text "ended during left"))
    reifyPInit' = Left (reifyPInit @('ParserSym plCh plEnd sl))

instance ReifyP End where
    type ReifyPS End = ()
    type ReifyPR End = ()
    reifyPCh'   = failCh "End" (Text "expected end of symbol")
    reifyPEnd'  = Right
    reifyPInit' = ()

instance (ReifyP ('ParserSym pCh pEnd s), KnownNat n)
  => ReifyP (Isolate'' n pCh pEnd s) where
    type ReifyPS (Isolate'' n pCh pEnd s) =
        (Natural, ReifyPS ('ParserSym pCh pEnd s))
    type ReifyPR (Isolate'' n pCh pEnd s) = ReifyPR ('ParserSym pCh pEnd s)
    reifyPCh'   = failCh "End" (Text "expected end of symbol")
    reifyPEnd' (n, s) =
        case n of
          0 ->
            case reifyPEnd @('ParserSym pCh pEnd s) s of
              Right r -> Right r
              Left  e -> Left (EIn "Isolate" e)
          _ -> Left (EBase "Isolate"
            (      Text "tried to isolate more than present (needed "
              :<>: Text (show n) :<>: Text " more)" ))
    reifyPInit' = (natVal'' @n, reifyPInit @('ParserSym pCh pEnd s))

natVal'' :: forall (n :: Natural). KnownNat n => Natural
natVal'' = natVal' (proxy# @n)
