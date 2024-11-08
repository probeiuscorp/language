{-# LANGUAGE FlexibleInstances #-}

module Compiler.SnapshotTesting (snapshot, snapshotData, SnapshotInput, showSnapshotInput) where
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

class SnapshotInput a where
  showSnapshotInput :: a -> String

instance SnapshotInput String where
  showSnapshotInput = id

snapshot :: SnapshotInput a => String -> (a -> String) -> String -> a -> SpecWith ()
snapshot directory f msg a = snapshotData directory msg (showSnapshotInput a, f a)

snapshotData :: String -> String -> (String, String) -> SpecWith ()
snapshotData directory msg (input, output) = it msg $ defaultGolden (directory ++ msg) fileContents
  where
    fileContents = input ++ "\n\x2500\x2500\x2500\n" ++ output ++ "\n"
