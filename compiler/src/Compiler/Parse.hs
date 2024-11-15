module Compiler.Parse where

import Compiler.Tokenizer
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Data.Char (isSpace)
import Compiler.Linearizer (Linear, GLinearized (..), linearize)
import Compiler.ParseInfix (parseInfix)
import Control.Monad (guard, msum, MonadPlus (mzero))
import Control.Monad.State (State, MonadState (state, get), gets, evalState, modify, runState, StateT (StateT, runStateT), execState)
import Data.Functor ((<&>), ($>))
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(first, bimap))
import Compiler.Zipper (filterMaybe)

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

parseDeclaration :: State Tokens AST.TopLevelDeclaration
parseDeclaration = do
  token <- right
  eatWhitespaceTokens
  z <- get
  case content <$> Z.peek z of
    Just "=" -> do
      term <- gets $ parseTerm . Z.start . linearize
      pure $ AST.ValueDeclaration (AST.DeclarationModule {
        AST.identifier = content token,
        AST.isExported = False
      }) (Just term) Nothing
    _ -> case content token of
      "type" -> undefined
      "export" -> undefined
      "import" -> parseImportDeclaration
      _ -> undefined

type ParseAttempt = StateT Tokens Maybe AST.ImportListing
parseImportDeclaration :: State Tokens AST.TopLevelDeclaration
parseImportDeclaration = do
  eatWhitespaceTokens
  specifier <- catTokens <$> state (Z.match . filterMaybe $ not . isWhitespace)
  eatWhitespaceTokens
  importListing <- gets . runStateT $ msum [matchAs, matchHiding, matchOnly, matchAll]
  pure . AST.ImportDeclaration specifier $ maybe undefined fst importListing
  where
    matchAs :: ParseAttempt
    matchAs = do
      keyword <- StateT Z.right
      guard $ content keyword == "as"
      liftState eatWhitespaceTokens
      binding <- StateT Z.right
      pure . AST.ImportAs $ content binding
    parseImportList :: StateT Tokens Maybe [(AST.ValidIdentifier, AST.ValidIdentifier)]
    parseImportList = do
      z <- get
      case linearize z of
        [LinBraces l] -> pure []
        _ -> mzero
    matchOnly :: ParseAttempt
    matchOnly = AST.ImportOnly <$> parseImportList
    matchHiding :: ParseAttempt
    matchHiding = do
      keyword <- StateT Z.right
      guard $ content keyword == "hiding"
      liftState eatWhitespaceTokens
      AST.ImportHiding <$> parseImportList
    matchAll :: ParseAttempt
    matchAll = do
      z <- get
      guard $ Z.isDone z
      pure AST.ImportAll
    liftState :: State s a -> StateT s Maybe a
    liftState f = StateT $ pure . runState f

eatWhitespaceTokens :: State Tokens ()
eatWhitespaceTokens = modify $ Z.eat isWhitespace

type ParseState = State Linear

right :: State (Z.Zipper a) a
right = state $ fromMaybe (error "Unexpected end of input") . Z.right

eatWhitespace :: ParseState Bool
eatWhitespace = state $ go False
  where
    go hadWhitespace z = fromMaybe (hadWhitespace, z) $ Z.right z >>= \(l, zr) -> case l of
      (LinToken t) | isWhitespace t -> Just $ go True zr
      _ -> Nothing

only :: ParseState a -> ParseState a
only match = do
  r <- match
  eatWhitespace
  isDone <- gets Z.isDone
  if isDone
    then pure r
    else unexpected

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
parseOneTerm z = Z.right z >>= \(lin, zr) -> let ok term = Just (term, zr) in case lin of
  (LinToken t) | isWhitespace t -> parseOneTerm zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> ok $ AST.TermIdentifier $ content t
  (LinParens l) -> ok $ parseParens $ Z.start l
  (LinBrackets l) -> ok $ AST.TermList $ either pure id $ parseCommaSeparated $ Z.start l
  (LinBraces l) -> ok $ parseRecordLiteral $ Z.start l
  (LinFunction lparams lbody) ->
    ok $ AST.TermFunction (evalState (exhaustively parseDestructuring) $ Z.start lparams) (parseTerm $ Z.start lbody)
  l -> error $ "term not supported yet: " ++ show l

breakWhen :: (Token -> Bool) -> Linear -> (Maybe Linear, Linear)
breakWhen p z0 = go z0
  where
    go :: Linear -> (Maybe Linear, Linear)
    go z = maybe (Nothing, z0) (\(l, zr) -> case l of
      LinToken t | p t -> (Just $ Z.restart zr, Z.start . reverse $ Z.done z)
      _ -> go zr) $ Z.right z

data ParenParseState = ExpectTuple [AST.Term] | ExpectGroup
parseCommaSeparated :: Linear -> Either AST.Term [AST.Term]
parseCommaSeparated = go ExpectGroup
  where
    go s z = case (s, zr) of
      (ExpectGroup, Nothing) -> Left term
      (ExpectGroup, Just xs) -> go (ExpectTuple $ pure term) xs
      (ExpectTuple terms, Nothing) -> Right . reverse $ term : terms
      (ExpectTuple terms, Just xs) -> go (ExpectTuple $ term : terms) xs
      where
        (zr, z') = breakWhen ((== ",") . content) z
        term = parseTerm z'
parseParens = either id AST.TermTuple . parseCommaSeparated

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

splitRecordClauses :: String -> Linear -> [(Linear, Maybe Linear)]
splitRecordClauses sep z0 = splitClauses z0 >>= parseClause
  where
    parseClause :: Linear -> [(Linear, Maybe Linear)]
    parseClause clause = case breakWhen ((== sep) . content) clause of
      (Just rhs, lhs) -> pure (execState eatWhitespace lhs, Just $ execState eatWhitespace rhs)
      (Nothing, lhs) -> let lhs' = execState eatWhitespace lhs in
        guard (not $ Z.isDone lhs') $> (lhs', Nothing)
    splitClauses z = let (mrest, segment) = breakWhen ((== ",") . content) z in
      case mrest of
        Just rest -> segment : splitClauses rest
        Nothing -> pure segment

braceParser :: (Linear -> a) -> (Linear -> b) -> String -> Linear -> [(a, Maybe b)]
braceParser fLHS fRHS sep z = bimap fLHS (fmap fRHS) <$> splitRecordClauses sep z

parseRecordLiteral = AST.TermRecord . braceParser (evalState $ only $ expect matchIdentifier) parseTerm "="
