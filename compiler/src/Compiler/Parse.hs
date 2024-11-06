module Compiler.Parse where

import Compiler.Tokenizer
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Data.Char (isSpace)
import Compiler.Linearizer (Linear, Linearized, GLinearized (..))
import Compiler.ParseInfix (parseInfix)

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

parseTerm :: Linear -> AST.Term
parseTerm = parseInfix parseOneTerm

parseOneTerm :: Linear -> Maybe (AST.Term, Linear)
parseOneTerm z = Z.right z >>= \(term, zr) -> case term of
  (LinToken t) | isWhitespace t -> parseOneTerm zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> Just (AST.TermIdentifier $ content t, zr)
  (LinParens l) -> Just (parseParens $ Z.start l, zr)
  l -> error $ "term not supported yet: " ++ show l

breakWhen :: (Token -> Bool) -> Linear -> Either Linear (Linear, Linear)
breakWhen p z = maybe (Left rewound) (\(l, zr) -> case l of
  LinToken t | p t -> Right (rewound, Z.restart zr)
  _ -> breakWhen p zr) $ Z.right z
  where rewound = Z.start . reverse $ Z.done z

data ParenParseState = ExpectTuple [AST.Term] | ExpectGroup
parseParens :: Linear -> AST.Term
parseParens = go ExpectGroup
  where
    go :: ParenParseState -> Linear -> AST.Term
    go s z = case (s, zr) of
      (ExpectGroup, Nothing) -> term
      (ExpectGroup, Just xs) -> go (ExpectTuple $ pure term) xs
      (ExpectTuple terms, Nothing) -> AST.TermTuple . reverse $ term : terms
      (ExpectTuple terms, Just xs) -> go (ExpectTuple $ term : terms) xs
      where
        (z', zr) = case breakWhen ((== ",") . content) z of
          Left z' -> (z', Nothing)
          Right (z', zr) -> (z', Just zr)
        term = parseTerm z'
