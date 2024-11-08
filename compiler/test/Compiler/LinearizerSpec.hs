module Compiler.LinearizerSpec (spec) where

import Test.Hspec
import qualified Compiler.Zipper as Z
import Compiler.Linearizer (linearize)
import Compiler.Tokenizer (tokenize, content)
import Compiler.SnapshotTesting (snapshot)

spec :: SpecWith ()
spec = describe "linearize" $ do
  let prettyPrintLinearization = show . fmap (fmap content) . linearize . Z.start . tokenize
  let test = snapshot "linearize/" prettyPrintLinearization
  test "parens" "a (b c d) e"
  test "nested parens" "a ((b c) d) e"
  test "braces" "a {b c d} e"
  test "nested braces" "a {{b c} d} e"
  test "mixed nestings" "(a {b c}) d (e)"
  test "functions" "x y. x"
  test "functions with parentheses" "x (Cons (Just x) xs). undefined"
