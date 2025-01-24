module Compiler.SemanticSpec where

import Test.Hspec
import Compiler.SnapshotTesting (snapshot, prettyShow)
import Compiler.Linearize (linearize)
import Compiler.Tokenize (tokenize)
import Compiler.Parse (parseTerm)
import qualified Compiler.Zipper as Z
import Compiler.Semantic (semanticValue)

spec :: SpecWith ()
spec = describe "Compiler.Semantic" $ do
  let test = snapshot "semantic/" $ prettyShow . semanticValue . parseTerm . Z.start . linearize . Z.start . tokenize
  test "finding free variables" "x y z. x z (y z)"
  test "erroring unknown variables" "x y. w"
  test "int" "-144"
  test "basic double" "-12.25"
  test "double with no fractions" "10e-2"
