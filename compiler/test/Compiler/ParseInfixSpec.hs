module Compiler.ParseInfixSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import qualified Compiler.Zipper as Z
import qualified Compiler.AST as AST
import Compiler.Parser
import Compiler.Tokenizer (tokenize)
import Compiler.Linearizer (linearize)
import Compiler.ParseInfix (parseInfix)

prettyPrintTerm :: String -> AST.Term -> String
prettyPrintTerm lastIndent (AST.TermApplication (AST.TermApplication fn arg1) arg2) =
  prettyPrintTerm lastIndent fn
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "\x2502 ") arg1
  ++ "\n" ++ indent ++ prettyPrintTerm (lastIndent ++ "  ") arg2
  where indent = lastIndent ++ "\x2937 "
prettyPrintTerm indent (AST.TermApplication fn arg) = "(" ++ prettyPrintTerm indent fn ++ " " ++ prettyPrintTerm indent arg ++ ")"
prettyPrintTerm indent (AST.TermIdentifier ident) = ident
prettyPrintTerm indent term = show term

spec = describe "parseInfix" $ do
  let prettyParseInfix = prettyPrintTerm "" . parseInfix . Z.start . linearize . Z.start . tokenize
  let test msg source = it msg $ defaultGolden ("parseInfix/" ++ msg) $ source ++ "\n\x2500\x2500\x2500\n" ++ prettyParseInfix source ++ "\n"
  test "right associativity" "a $ b $ c $ d"
  test "left associativity" "a - b - c - d"
  test "application" "a b c d"
  test "left associative same precedence" "a / b / c * d * e / f"
  test "right associative same precedence" "a ^ b ^^ c ^ d ^^ e ^^ f"
  test "mixed 1" "a b $ c d $ e"
  test "mixed 2" "d0 - d1 - d2 * a * b $ x y z"
  test "mixed 3" "d * a $ b b0 b1 $ c $ d x y z - e - f $ g"
  test "single term" "a"

