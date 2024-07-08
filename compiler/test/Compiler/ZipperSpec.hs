module Compiler.ZipperSpec (spec) where

import Test.Hspec
import qualified Compiler.Zipper as Z

spec :: SpecWith ()
spec = describe "Compiler.Zipper" $ do
  it "should eat while predicate matches" $ do
    Z.peek (Z.eat (==0) $ Z.start [0, 0, 0, 0, 1, 2]) `shouldBe` Just 1
  it "match should preserve order" $ do
    let z = Z.start [0, 1, 2, 3, 4, 5] in
      Z.matchCond (< 3) z `shouldBe` ([0, 1, 2], Z.Zipper [2, 1, 0] [3, 4, 5])
