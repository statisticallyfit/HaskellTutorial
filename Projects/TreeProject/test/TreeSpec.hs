module TreeSpec (main, spec) where

import Tree

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
