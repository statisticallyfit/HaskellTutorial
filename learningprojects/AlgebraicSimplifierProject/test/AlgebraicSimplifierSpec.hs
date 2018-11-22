module AlgebraicSimplifierSpec (main, spec) where

import AlgebraicSimplifier

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
    testStub

testStub :: SpecWith ()
testStub = describe "this should tell what happens" $

    it "spec1" $ 1 == 0 + 1
