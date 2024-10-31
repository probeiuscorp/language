module Compiler.ParserSpec (spec) where

import Test.Hspec
import Compiler.Parser
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenizer (tokenize)

spec :: SpecWith ()
spec = describe "Compiler.Parser" $ do
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
