module Compiler.TokenizerSpec (spec) where

import Test.Hspec
import Compiler.Tokenizer (tokenize)
import Compiler.SnapshotTesting (snapshotData)
import Data.List (intercalate)

spec :: SpecWith ()
spec = describe "Compiler.Tokenizer" $ do
  let prettyTokenize = intercalate "\n" . fmap show . tokenize
  let test testCase contents = snapshotData "tokenize/" testCase (show contents, prettyTokenize contents)
  test "simple example" "export main:IO()\t=of()\n"
  test "parens" ")::())("
  test "braces" "}::{)}{"
  test "math symbols" "∀x. x + x==2x"
  test "newlines in whitespace" "  \n  "
