{-# LANGUAGE LambdaCase #-}

module Compiler.Parse where

import Compiler.Tokenizer
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Data.Char (isSpace)
import Compiler.Linearizer (Linear, Linearized, GLinearized (..))
import Compiler.ParseInfix (parseInfix)
import Control.Monad.State (State, MonadState (state, get), evalState)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(first))

type Tokens = Z.Zipper Token

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) f = (fmap f <$>)
infixl 4 <<$>>

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

type ParseState = State Linear

right :: ParseState Linearized
right = state $ fromMaybe (error "Unexpected end of input") . Z.right

eatWhitespace :: ParseState Bool
eatWhitespace = state $ go False
  where
    go hadWhitespace z = fromMaybe (hadWhitespace, z) $ Z.right z >>= \(l, zr) -> case l of
      (LinToken t) | isWhitespace t -> Just $ go True zr
      _ -> Nothing

many :: ParseState (Maybe a) -> ParseState [a]
many match = go []
  where
    go xs = match >>= \case
      Just x -> go $ x:xs
      Nothing -> pure xs

exhaustively :: ParseState a -> ParseState [a]
exhaustively match = reverse <$> go []
  where
    go xs = do
      z <- get
      if Z.isDone z
        then pure xs
        else match >>= go . (:xs)

unexpected :: ParseState a
unexpected = pure $ error "Did not get what expected"

expect :: ParseState (Maybe a) -> ParseState a
expect = (>>= \case
  Just x -> pure x
  Nothing -> unexpected)

matchIdentifier :: ParseState (Maybe AST.ValidIdentifier)
matchIdentifier = right <&> \case
  LinToken t | kind t == LetterIdentifier -> Just $ content t
  _ -> Nothing

parseTerm :: Linear -> AST.Term
parseTerm = parseInfix parseOneTerm

parseOneTerm :: Linear -> Maybe (AST.Term, Linear)
parseOneTerm z = Z.right z >>= \(term, zr) -> case term of
  (LinToken t) | isWhitespace t -> parseOneTerm zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> Just (AST.TermIdentifier $ content t, zr)
  (LinParens l) -> Just (parseParens $ Z.start l, zr)
  l -> error $ "term not supported yet: " ++ show l

breakWhen :: (Token -> Bool) -> Linear -> (Maybe Linear, Linear)
breakWhen p z0 = go z0
  where
    go :: Linear -> (Maybe Linear, Linear)
    go z = maybe (Nothing, z0) (\(l, zr) -> case l of
      LinToken t | p t -> (Just $ Z.restart zr, Z.start . reverse $ Z.done z)
      _ -> go zr) $ Z.right z

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
        (zr, z') = breakWhen ((== ",") . content) z
        term = parseTerm z'

parseDestructuring :: ParseState AST.Destructuring
parseDestructuring = eatWhitespace >> right >>= \case
  (LinToken t) | kind t == LetterIdentifier -> pure . AST.DestructBind $ content t
  (LinParens l) -> pure . evalState parseNominal $ Z.start l
  _ -> unexpected

parseNominal :: ParseState AST.Destructuring
parseNominal = do
  identifier <- expect matchIdentifier
  destructurings <- exhaustively parseDestructuring
  pure $ AST.DestructNominal (identifier, destructurings)
