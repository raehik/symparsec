module Main where

import Test.TypeSpec
import Data.Type.Symbol.Parser

main :: IO ()
main = print spec

-- The type errors for failures are HILARIOUS if you're into that sort of thing.
-- Try messing with a test or two and see mad GHC gets!

type CstrX_Y =
          (Literal "Cstr" :*>: Isolate 2 NatDec)
    :<*>: (Literal "_"    :*>: Isolate 2 NatHex)

spec :: Expect
    '[ Run (Literal "raehik") "raehik" `Is` Right '( '(), "")
     , Run (Literal "raeh") "raehraeh" `Is` Right '( '(), "raeh")
     , Run (Drop 3 :*>: Literal "HI") "...HI" `Is` Right '( '(), "")
     , Run (Literal "0x" :*>: NatHex) "0xfF" `Is` Right '( 255, "")
     , Run CstrX_Y "Cstr12_AB" `Is` Right '( '(12, 0xAB), "")
     ]
spec = Valid
