module Compiler.ParseSpec (spec, prettyPrintTerm) where

import Test.Hspec
import Compiler.Parse
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenizer (tokenize)
import Compiler.Linearizer (linearize)
import Compiler.SnapshotTesting (snapshot)

prettyPrintTerm :: String -> AST.Term -> String
prettyPrintTerm lastIndent (AST.TermApplication (AST.TermApplication fn arg1) arg2) =
  prettyPrintTerm lastIndent fn
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "\x2502 ") arg1
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "  ") arg2
  where indent = lastIndent ++ "\x2937 "
prettyPrintTerm indent (AST.TermApplication fn arg) = "(" ++ prettyPrintTerm indent fn ++ " " ++ prettyPrintTerm indent arg ++ ")"
prettyPrintTerm _ (AST.TermIdentifier ident) = ident
prettyPrintTerm _ term = show term

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
  let source = Z.start . linearize . Z.start . tokenize
  describe "parseParens" $ do
    let prettyParseParens = show . parseParens . source
    let test = snapshot "parse/parseParens/" prettyParseParens
    test "single group" "(this)"
    test "tree group" "(this that there)"
    test "tuple 2" "(this, that)"
    test "tuple 3" "(this, that, there)"
  describe "parseDestructuring" $ do
    let prettyParseDestructuring = show . parseParens . source
    let test = snapshot "parse/parseDestructuring/" prettyParseDestructuring
    test "binding" "x"
    test "nominal" "(None)"
    test "nominal many" "(Cons x xs)"
    test "nominal nested" "(Cons (Nothing) (Just x))"
