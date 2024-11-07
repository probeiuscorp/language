module Compiler.ParseSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Compiler.Parse
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenizer (tokenize)
import Compiler.Linearizer (linearize)

spec :: SpecWith ()
spec = describe "Compiler.Parse" $ do
  describe "splitDeclarations" $ do
    let testDeclarations = splitDeclarations . Z.start
    it "empty string" $ do
      testDeclarations "" `shouldBe` []
    it "just newlines" $ do
      testDeclarations "\n\n\n\n\n\n" `shouldBe` []
    it "should continue if inline whitespace" $ do
      testDeclarations "pair = a.\n\t(a, a)" `shouldBe` ["pair = a.\n\t(a, a)"]
    it "should continue if closing brace" $ do
      let source = "import react/hooks {\n\tuseState\n}\n"
      testDeclarations source `shouldBe` [source]
    it "should continue if closing parenthesis" $ do
      let source = "pair = a. (\n\ta,\n\ta)\n"
      testDeclarations source `shouldBe` [source]
  describe "parseParens" $ do
    let prettyParseParens = show . parseParens . Z.start . linearize . Z.start . tokenize
    let test msg source = it msg $ defaultGolden ("parse/parseParens/" ++ msg) $ source ++ "\n\x2500\x2500\x2500\n" ++ prettyParseParens source ++ "\n"
    test "single group" "(this)"
    test "tree group" "(this that there)"
    test "tuple 2" "(this, that)"
    test "tuple 3" "(this, that, there)"
  describe "parseDestructuring" $ do
    let prettyParseDestructuring = show . parseParens . Z.start . linearize . Z.start . tokenize
    let test msg source = it msg $ defaultGolden ("parse/parseDestructuring/" ++ msg) $ source ++ "\n\x2500\x2500\x2500\n" ++ prettyParseDestructuring source ++ "\n"
    test "binding" "x"
    test "nominal" "(None)"
    test "nominal many" "(Cons x xs)"
    test "nominal nested" "(Cons (Nothing) (Just x))"
