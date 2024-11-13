module Compiler.LinearizerSpec (spec) where

import Test.Hspec
import qualified Compiler.Zipper as Z
import Compiler.Linearizer (linearize, Linearized, GLinearized (..))
import Compiler.Tokenizer (tokenize, content)
import Compiler.SnapshotTesting (snapshot)

prettyPrintLinearization :: [Linearized] -> String
prettyPrintLinearization l = "[\n" ++ go "" l ++ "]"
  where
    go :: String -> [Linearized] -> String
    go indent lbody = lbody >>= prettyPrintLinearized ("  " ++ indent)
    prettyPrintLinearized :: String -> Linearized -> String
    prettyPrintLinearized indent t@(LinToken _) = indent ++ (show . fmap content) t ++ ",\n"
    prettyPrintLinearized indent (LinFunction lparams lbody) = indent ++ "LinFunction" ++ showLinearization indent lparams ++ showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinParens lbody)   = indent ++ "LinParens" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBraces lbody)   = indent ++ "LinBraces" ++   showLinearization indent lbody ++ ",\n"
    prettyPrintLinearized indent (LinBrackets lbody) = indent ++ "LinBrackets" ++ showLinearization indent lbody ++ ",\n"
    showLinearization :: String -> [Linearized] -> String
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
