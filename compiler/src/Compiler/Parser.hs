{-# LANGUAGE MultiWayIf #-}

module Compiler.Parser where

import Compiler.Tokenizer
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Text.Printf (printf)
import Data.Maybe (fromJust, fromMaybe)
import Control.Applicative (Alternative((<|>)))
import Data.Functor ((<&>))
import Control.Arrow ((>>>))
import Data.Char (isSpace)

type Tokens = Z.Zipper Token
data ParseError = ParseError String Token deriving (Eq, Show)
type ParseResult = Either ParseError
type ParseMatcher a = Tokens -> Maybe (ParseResult (a, Tokens))
type ParseParser a = Tokens -> ParseResult (a, Tokens)

parseFail :: String -> Tokens -> ParseError
parseFail message tokens = ParseError message $ fromJust $ Z.peek tokens

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
doesDeclarationContinue ch  = isInlineWhitespace ch

parseDeclaration :: ParseParser AST.TopLevelDeclaration
parseDeclaration tokens = fromMaybe (Left $ parseFail (printf "Unexpected token") tokens) $
      parseImportDeclaration tokens
  <|> parseExportDeclaration tokens

parseImportDeclaration :: ParseMatcher AST.TopLevelDeclaration
parseImportDeclaration tokens = matchKeyword "import" tokens <&> (eatWhitespace
  >>> Z.matchCond (not . isWhitespace)
  >>> \(specifierTokens, importListTokens) -> parseImportList importListTokens
    <&> mapFirst (AST.ImportDeclaration $ catTokens specifierTokens))

parseImportList :: ParseParser AST.ImportListing
parseImportList tokens = fromMaybe (Right (AST.ImportAll, tokens)) $
      parseImportListAs tokens
  <|> parseImportListHiding tokens
  <|> parseImportListOnly tokens

parseImportListAs :: ParseMatcher AST.ImportListing
parseImportListAs tokens = matchKeyword "as" tokens
  <&> fmap (mapFirst AST.ImportAs) . parseLetterIdentifier

parseImportListHiding :: ParseMatcher AST.ImportListing
parseImportListHiding = const Nothing

parseImportListOnly :: ParseMatcher AST.ImportListing
parseImportListOnly = const Nothing

parseExportDeclaration :: ParseMatcher AST.TopLevelDeclaration
parseExportDeclaration = undefined

eatWhitespace :: Tokens -> Tokens
eatWhitespace = Z.eat isWhitespace

matchKeyword :: String -> Tokens -> Maybe Tokens
matchKeyword keyword z = Z.right z >>= \(x,zr) -> if
  | content x == keyword -> Just zr
  | isWhitespace x       -> matchKeyword keyword zr
  | otherwise            -> Nothing

expectKeyword :: String -> Tokens -> ParseResult Tokens
expectKeyword keyword tokens = maybe e Right $ matchKeyword keyword tokens
  where
    start = printf "Expected keyword \"%s\"" keyword :: String
    message = maybe "Unexpected end of input" (printf "%s but got \"%s\"" start . content) $ Z.peek tokens
    e = Left $ parseFail message tokens

parseLetterIdentifier :: ParseParser AST.ValidIdentifier
parseLetterIdentifier tokens = Right ("Hooks", tokens)

parseExpression :: [Token] -> (AST.Expression, [Token])
parseExpression (Token { kind = StringLiteral content }:xs) = (AST.StringLiteral content, xs)
