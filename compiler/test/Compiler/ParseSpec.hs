module Compiler.ParseSpec (spec, prettyPrintTerm) where

import Test.Hspec
import Compiler.Parse
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Tokenize (tokenize)
import Compiler.Linearize (linearize)
import Compiler.SnapshotTesting (snapshot)
import Control.Monad.State (evalState)

prettyPrintTerm :: String -> AST.Term -> String
prettyPrintTerm lastIndent (AST.TermApplication (AST.TermApplication fn arg1) arg2) =
  prettyPrintTerm lastIndent fn
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "\x2502 ") arg1
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "  ") arg2
  where indent = lastIndent ++ "\x2937 "
prettyPrintTerm indent (AST.TermApplication fn arg) = "(" ++ prettyPrintTerm indent fn ++ " " ++ prettyPrintTerm indent arg ++ ")"
prettyPrintTerm _ (AST.TermIdentifier ident) = ident
prettyPrintTerm lastIndent (AST.TermFunction params body) = "TermFunction " ++ show params ++ " (\n" ++ indent ++ prettyPrintTerm indent body ++ "\n" ++ lastIndent ++ ")"
  where indent = "  " ++ lastIndent
prettyPrintTerm indent (AST.TermRecord fields) = "TermRecord {\n" ++ (fields >>= showField) ++ indent ++ "}"
  where
    nextIndent = "  " ++ indent
    showValue Nothing = "Nothing"
    showValue (Just term) = prettyPrintTerm nextIndent term
    showField (name, value) = nextIndent ++ name ++ " = " ++ showValue value ++ "\n"
prettyPrintTerm indent (AST.TermMatch clauses) = "TermMatch [\n" ++ (clauses >>= showField) ++ indent ++ "]"
  where
    nextIndent = "  " ++ indent
    showField (destructurings, term) = nextIndent ++ show destructurings ++ " = " ++ prettyPrintTerm nextIndent term ++ "\n"
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
  describe "parseImportDeclaration" $ do
    let prettyParseImport = show . evalState parseDeclaration . Z.start . tokenize
    let test = snapshot "parse/parseImport/" prettyParseImport
    test "import all" "import react/hooks"
    test "import only" "import react/hooks {}"
    test "import only list" "import react/hooks { useState, useEffect }"
    test "import hiding list" "import react/hooks hiding { useState, useEffect }"
    test "import as" "import react/hooks as ReactHooks"
  let source = Z.start . linearize . Z.start . tokenize
  describe "parseDestructuring" $ do
    let prettyParseDestructuring = show . evalState parseDestructuring . source
    let test = snapshot "parse/parseDestructuring/" prettyParseDestructuring
    test "binding" "x"
    test "nominal" "(None)"
    test "nominal many" "(Cons x xs)"
    test "nominal nested" "(Cons (Nothing) (Just x))"
    test "record" "{ x, y, z }"
    test "record rebind" "{ x as playerX, y as playerY }"
    test "record destructure" "{ start as { x as x0, y as y0 }, end as { x as x1, y as y1 } }"
  describe "parseTerm" $ do
    let prettyParseTerm = prettyPrintTerm "" . parseTerm . source
    let test = snapshot "parse/parseTerm/" prettyParseTerm
    test "identity function" "x. x"
    test "multiple bindings function" "x y. x"
    test "single group" "(this)"
    test "tree group" "(this that there)"
    test "tuple 2" "(this, that)"
    test "tuple 3" "(this, that, there)"
    test "list one item" "[x]"
    test "list multiple items" "[x, y, z]"
    test "nested destructurings" "x (Cons (Nothing) (Just x)). undefined"
    test "record literal"
      "{\n\
      \  x = xx. undefined,\n\
      \  y = yy,\n\
      \  z,\n\
      \  d = dd, e = ee\n\
      \  ,another,\n\
      \}"
    test "record literal one line no trailing comma" "{ a, b, c = undefined, d }"
    test "record literals nested" "{ a, b = { this }, c, d = {}}"
    test "record literal in function" "x y. { x, y }"
    test "match empty" "match { }"
    test "match simple"
      "f. match {\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n\
      \}"
    test "match trailers"
      "match {\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n\
      \} list"
    test "match no braces"
      "f. match\n\
      \  (Cons x xs) = f x $ xs\n\
      \  (Nil) = Nil\n"
