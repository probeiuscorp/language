module Compiler.LinearizeSpec (spec) where

import Test.Hspec
import qualified Compiler.Zipper as Z
import Compiler.Linearize (linearize, Linearized, Linearization, GLinearized (..))
import Compiler.Tokenize (tokenize, content)
import Compiler.SnapshotTesting (snapshot)

prettyPrintLinearization :: [Linearized] -> String
prettyPrintLinearization l = "[\n" ++ go "" l ++ "]"
  where
    go :: String -> Linearization -> String
    go indent lbody = lbody >>= prettyPrintLinearized ("  " ++ indent)
    prettyPrintLinearized :: String -> Linearized -> String
    prettyPrintLinearized indent t@(LinToken _) = indent ++ (show . fmap content) t ++ ",\n"
    prettyPrintLinearized indent (LinFunction lparams lbody) = indent ++ "LinFunction" ++ showLinearization indent lparams ++ showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinParens lbody)   = indent ++ "LinParens" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBraces lbody)   = indent ++ "LinBraces" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBrackets lbody) = indent ++ "LinBrackets" ++ showLinearization indent lbody ++ ",\n"
    showLinearization :: String -> Linearization -> String
    showLinearization indent lbody = " [\n" ++ go indent lbody ++ indent ++ "]"

spec :: SpecWith ()
spec = describe "linearize" $ do
  let prettyParseLinearization = prettyPrintLinearization . linearize . Z.start . tokenize
  let test = snapshot "linearize/" prettyParseLinearization
  test "parens" "a (b c d) e"
  test "nested parens" "a ((b c) d) e"
  test "braces" "a {b c d} e"
  test "nested braces" "a {{b c} d} e"
  test "mixed nestings" "(a {b c}) d (e)"
  test "functions" "x y. x"
  test "functions with parentheses" "x (Cons (Just x) xs). undefined"
  test "function in parentheses" "(x y. x)"
  test "function with comma in parenthesis in body" "(x y. x (x y, y) z)"
  test "functions in tuples" "(x y. x, x y. y)"
  test "functions in tuples with trailing comma" "(x y. x, x y. y,)"
  test "functions in brace rhs"
    "{\n\
    \  a = x y. x,\n\
    \  b = x y. y,\n\
    \  c = undefined,\n\
    \}"
  test "functions in brace rhs with trailing comma"
    "{\n\
    \  a = x y. x,\n\
    \  b = undefined,\n\
    \  c = x y. y,\n\
    \}"
  test "functions in brace rhs no trailing comma"
    "{\n\
    \  a = x y. x,\n\
    \  b = undefined,\n\
    \  c = x y. y\n\
    \}"
