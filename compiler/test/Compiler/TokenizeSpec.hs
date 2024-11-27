module Compiler.TokenizeSpec (spec) where

import Test.Hspec
import Compiler.Tokenize
import Compiler.SnapshotTesting (snapshotData)
import Data.List (intercalate)
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple (OutputOptions (outputOptionsIndentAmount), defaultOutputOptionsNoColor, pShowOpt)
import Control.Monad (forM_)

options :: OutputOptions
options = defaultOutputOptionsNoColor
  { outputOptionsIndentAmount = 2 }
prettyShowToken :: Token -> String
prettyShowToken token@(Token { kind = NumberLiteral _ }) = unpack $ pShowOpt options token
prettyShowToken token = show token

spec :: SpecWith ()
spec = describe "Compiler.Tokenize" $ do
  let prettyTokenize = intercalate "\n" . fmap prettyShowToken . tokenize
  let mkTest dir testCase contents = snapshotData dir testCase (show contents, prettyTokenize contents)
  do
    let test = mkTest "tokenize/"
    test "simple example" "export main:IO()\t=of()\n"
    test "parens" ")::())("
    test "braces" "}::{)}{"
    test "math symbols" "∀x. x + x==2x"
    test "newlines in whitespace" "  \n  "
  describe "numbers" $ do
    let test' = mkTest "tokenize/number/"
    forM_ [("decimal", "1728"), ("hex", "0x12B0"), ("octal", "0o27"), ("binary", "0b01101001")] $ \(name, value) -> do
      let test label = test' (label ++ " " ++ name)
      test "integer unsigned" value
      test "integer negative" ("-" ++ value)
      test "integer explicitly positive" ("+" ++ value)
    let test = test'
    test "exponent decimal" "1e12"
    test "exponent-like hex" "0x10e6"
    test "fractional decimal" "10.25"
    test "fractional hex" "0xFF.8"
    test "among others" "y = x. 0.5 * x + 12"