module Compiler.ParseInfixSpec (spec) where

import Test.Hspec
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Parse (parseOneTerm)
import Compiler.Tokenize (tokenize)
import Compiler.Linearize (linearize)
import Compiler.ParseInfix (parseInfix)
import Compiler.ParseSpec (prettyPrintTerm, testAboutOperators)
import Compiler.SnapshotTesting (snapshot)

spec :: SpecWith ()
spec = describe "parseInfix" $ do
  let prettyParseInfix = prettyPrintTerm "" . parseInfix (testAboutOperators, parseOneTerm testAboutOperators) . Z.start . linearize . Z.start . tokenize
  let test = snapshot "parseInfix/" prettyParseInfix
  test "right associativity" "a $ b $ c $ d"
  test "left associativity" "a - b - c - d"
  test "application" "a b c d"
  test "left associative same precedence" "a / b / c * d * e / f"
  test "right associative same precedence" "a ^ b ^^ c ^ d ^^ e ^^ f"
  test "mixed 1" "a b $ c d $ e"
  test "mixed 2" "d0 - d1 - d2 * a * b $ x y z"
  test "mixed 3" "d * a $ b b0 b1 $ c $ d x y z - e - f $ g"
  test "single term" "a"
  test "non associative alone" "a \\ b"
  test "non associative complex" "a $ b c \\ d $ e"
  test "prefix operator simple" "~a"
  test "prefix operator simple after function" "f ~a"
  test "prefix operator consecutive" "~ ~ a"
  test "prefix operator repeated" "union $ ~ ¬ a ¬ ~ b"
  test "prefix operator simple multi argument" "union ~a ¬b"
  test "prefix operator complex multi argument" "union ~ ¬ a ¬ ~ b"
  test "postfix operators" "a ? ! + b ! ?"
  test "ignore first operator"
    "\n\
    \  + ErrorNotProvided\n\
    \  + ErrorNotInteger\n\
    \  + ErrorNotInRange"
  test "ignore first operator even after another operator"
    "combinator $\n\
    \  + ErrorNotProvided\n\
    \  + ErrorNotInteger\n\
    \  + ErrorNotInRange"
