module Compiler.SemanticSpec where

import Test.Hspec
import Compiler.SnapshotTesting (snapshot, prettyShow)
import Compiler.ParseSpec (testAboutOperators)
import Compiler.Linearize (linearize)
import Compiler.Tokenize (tokenize)
import Compiler.Parse (parseTerm)
import qualified Compiler.Zipper as Z
import Compiler.Semantic (semanticValue)

spec :: SpecWith ()
spec = describe "Compiler.Semantic" $ do
  let test = snapshot "semantic/" $ prettyShow . semanticValue mempty . parseTerm testAboutOperators . Z.start . linearize . Z.start . tokenize
  test "finding free variables" "x y z. x z (y z)"
  test "erroring unknown variables" "x y. w"
  test "int" "-144"
  test "basic double" "-12.25"
  test "double with no fractions" "10e-2"
  test "record expression" "{ x = 5, y = x x }"
  test "record expression shorthand" "y. { x = 5, y }"
  test "record expression shorthand unknown" "{ x = 5, y }"
  test "match expression"
    "Some None. match {\n\
    \  (Some x) = x\n\
    \  (None) = None\n}"
