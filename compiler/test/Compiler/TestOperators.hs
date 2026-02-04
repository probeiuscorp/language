module Compiler.TestOperators (testOperators) where

import qualified Compiler.AST as AST

opFixity :: String -> AST.Fixity
opFixity "¬" = AST.FixityPrefix
opFixity "~" = AST.FixityPrefix
opFixity "!" = AST.FixityPostfix
opFixity "?" = AST.FixityPostfix
opFixity ident = AST.FixityInfix $ AST.Infix (getOpPrecedence ident) (getOpAssociativity ident)

getOpPrecedence :: String -> Double
getOpPrecedence "$" = 1
getOpPrecedence "-" = 4
getOpPrecedence "^" = 8
getOpPrecedence "^^" = 8
getOpPrecedence _ = 6

getOpAssociativity :: String -> AST.Associativity
getOpAssociativity "$" = AST.RightAssociative
getOpAssociativity "-" = AST.LeftAssociative
getOpAssociativity "*" = AST.LeftAssociative
getOpAssociativity "/" = AST.LeftAssociative
getOpAssociativity "\\" = AST.NonAssociative
getOpAssociativity _ = AST.RightAssociative

testOperators :: AST.AboutOperators
testOperators = Just . opFixity
