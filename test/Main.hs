module Main where

import Test.TypeSpec
import Symparsec
import DeFun.Core ( type Con2 )

main :: IO ()
main = print spec

type CstrX_Y = LiftA2 (Con2 '(,))
    (Literal "Cstr" *> Isolate 2 NatDec)
    (Literal "_"    *> Isolate 2 NatHex)

spec :: Expect
    '[ Run' (Literal "raehik") "raehik" `Is` Right '( '(), "")
     , Run' (Literal "raeh") "raehraeh" `Is` Right '( '(), "raeh")
     , Run' (Skip 3 *> Literal "HI") "...HI" `Is` Right '( '(), "")
     , Run' (Literal "0x" *> NatHex) "0xfF" `Is` Right '( 255, "")
     , Run' CstrX_Y "Cstr12_AB" `Is` Right '( '(12, 0xAB), "")
     ]
spec = Valid
