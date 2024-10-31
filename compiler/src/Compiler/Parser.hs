module Compiler.Parser where

import Compiler.Tokenizer
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Data.Char (isSpace)
import Compiler.Linearizer (Linear, GLinearized (..))

type Tokens = Z.Zipper Token

isInlineWhitespace :: Char -> Bool
isInlineWhitespace '\n' = False
isInlineWhitespace ch   = isSpace ch

splitDeclarations :: Z.Zipper Char -> [String]
splitDeclarations z = filter (not . all isSpace) $ case Z.eatOne $ Z.eat (/= '\n') z of
  (Z.Zipper bt (ch:xs)) -> if doesDeclarationContinue ch
    then splitDeclarations $ Z.Zipper (ch:bt) xs
    else reverse bt : splitDeclarations (Z.Zipper [ch] xs)
  (Z.Zipper bt _) -> [reverse bt]

doesDeclarationContinue :: Char -> Bool
doesDeclarationContinue ')' = True
doesDeclarationContinue '}' = True
doesDeclarationContinue ']' = True
doesDeclarationContinue ch  = isInlineWhitespace ch

parseOneTerm :: Linear -> Maybe (AST.Term, Linear)
parseOneTerm z = Z.right z >>= (\(term, zr) -> case term of
  (LinToken t) | isWhitespace t -> parseOneTerm zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> Just (AST.TermIdentifier $ content t, zr)
  (LinBraces l) -> undefined
  l -> error $ "term not supported yet: " ++ show l
  )
