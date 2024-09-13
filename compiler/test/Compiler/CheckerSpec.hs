module Compiler.CheckerSpec where

import Test.Hspec
import Compiler.Type
import Compiler.Checker
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

nominal :: TypeNominal -> Type
nominal n = Type {
  typeNominal = n,
  typeRecords = TypeRecord [],
  typeConstructors = TypeConstructors Nothing
}
record :: TypeRecord -> Type
record r = Type {
  typeNominal = TypeNominal Set.empty,
  typeRecords = r,
  typeConstructors = TypeConstructors Nothing
}

unknown = Type {
  typeNominal = TypeNominalComplement Set.empty,
  typeRecords = TypeRecordComplement [],
  typeConstructors = TypeConstructorsComplement Nothing
}
never = Type {
  typeNominal = TypeNominal Set.empty,
  typeRecords = TypeRecord [],
  typeConstructors = TypeConstructors Nothing
}

spec :: SpecWith ()
spec = describe "Compiler.Checker" $ do
  describe "intersectPositiveWithNegative" $ do
    it "should intersect negatives with positive" $ do
      intersectPositiveWithNegative (group [
          ("a", unknown),
          ("b", unknown),
          ("c", unknown)
        ]) (Map.fromList <$> [[
          ("a", unknown)
        ], [
          ("b", unknown)
        ]]) `shouldBe` TypeRecord (group [
          ("c", unknown)
        ])
  describe "intersectType" $ do
    it "should narrow fields" $ do
      intersectType (posrecord [
          ("a", unknown),
          ("b", never)
        ]) (posrecord [
          ("a", never),
          ("c", unknown)
        ]) `shouldBe` posrecord [
          ("a", never)
        ]
  where
    group = pure . Map.fromList
    srecord f = record . f . group
    posrecord = srecord TypeRecord
    negrecord = srecord TypeRecordComplement
