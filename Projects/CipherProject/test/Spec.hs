module Spec where

import Test.Hspec
import Test.QuickCheck

import TestCaesarCipher
-- import TestVigniereCipher

main :: IO ()
main = TestCaesarCipher.runTests 
