module Compiler.Parse where

import Compiler.Tokenize
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Data.Char (isSpace)
import Compiler.Linearize (Linear, GLinearized (..), linearize)
import Compiler.ParseInfix (parseInfix)
import Control.Monad (guard, msum, MonadPlus (mzero))
import Control.Monad.State (State, MonadState (state, get, put), gets, evalState, modify, runState, StateT (StateT, runStateT), execState)
import Data.Functor ((<&>), ($>))
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (Bifunctor(first, bimap, second))
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
    parseImportList :: StateT Tokens Maybe AST.Destructuring
    parseImportList = do
      z <- get
      case linearize z of
        [LinBraces l] -> pure $ parseRecordDestructure $ Z.start l
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
      eatWhitespace
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
  (LinToken t) | content t == "match" -> Just $ runState parseMatch zr
  (LinToken t) | kind t == LetterIdentifier || kind t == SymbolIdentifier -> ok $ AST.TermIdentifier $ content t
  (LinParens l) -> ok $ parseParens $ Z.start l
  (LinBrackets l) -> ok $ AST.TermList $ either (pure . Just) id $ parseCommaSeparated $ Z.start l
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

data ParenParseState = ExpectTuple [Maybe AST.Term] | ExpectGroup
parseCommaSeparated :: Linear -> Either AST.Term [Maybe AST.Term]
parseCommaSeparated = go ExpectGroup
  where
    go :: ParenParseState -> Linear -> Either AST.Term [Maybe AST.Term]
    go s z = case (s, zr) of
      (ExpectGroup, Nothing) -> maybe (Right []) Left term
      (ExpectGroup, Just xs) -> go (ExpectTuple $ pure term) xs
      (ExpectTuple terms, Nothing) -> Right . reverse $ term : terms
      (ExpectTuple terms, Just xs) -> go (ExpectTuple $ term : terms) xs
      where
        (zr, z') = second (execState eatWhitespace) $ breakWhen (is ",") z
        term = if Z.isDone z' then Nothing else Just $ parseTerm z'
parseParens = either id AST.TermTuple . parseCommaSeparated

parseDestructuring :: ParseState AST.Destructuring
parseDestructuring = eatWhitespace >> right >>= \case
  (LinToken t) | kind t == LetterIdentifier -> pure . AST.DestructBind $ content t
  (LinParens l) -> pure . evalState parseNominal $ Z.start l
  (LinBraces l) -> pure $ parseRecordDestructure $ Z.start l
  _ -> unexpected

parseNominal :: ParseState AST.Destructuring
parseNominal = do
  identifier <- expect matchIdentifier
  destructurings <- exhaustively parseDestructuring
  pure $ AST.DestructNominal (identifier, destructurings)

splitClauses :: String -> Linear -> NE.NonEmpty Linear
splitClauses sep = go
  where
    go z = let (mrest, segment) = breakWhen ((== sep) . content) z in
      case mrest of
        Just rest -> segment NE.<| go rest
        Nothing -> pure segment
splitRecordClauses :: String -> Linear -> [(Linear, Maybe Linear)]
splitRecordClauses sep z0 = NE.toList (splitClauses "," z0) >>= parseClause
  where
    parseClause :: Linear -> [(Linear, Maybe Linear)]
    parseClause clause = case breakWhen ((== sep) . content) clause of
      (Just rhs, lhs) -> pure (execState eatWhitespace lhs, Just $ execState eatWhitespace rhs)
      (Nothing, lhs) -> let lhs' = execState eatWhitespace lhs in
        guard (not $ Z.isDone lhs') $> (lhs', Nothing)

braceParser :: (Linear -> a) -> (Linear -> b) -> String -> Linear -> [(a, Maybe b)]
braceParser fLHS fRHS sep z = bimap fLHS (fmap fRHS) <$> splitRecordClauses sep z

onlyIdentifier = evalState $ only $ expect matchIdentifier
parseRecordLiteral = AST.TermRecord . braceParser onlyIdentifier parseTerm "="
parseRecordDestructure = AST.DestructRecord . braceParser onlyIdentifier (evalState parseDestructuring) "as"

revZ :: Linear -> Linear
revZ (Z.Zipper bt ft) = Z.Zipper (reverse bt) (reverse ft)
rtl :: (Linear -> (Maybe Linear, Linear)) -> Linear -> (Maybe Linear, Linear)
rtl f z = bimap (fmap revZ) revZ $ f (revZ z)
parseMatchClauses :: Linear -> AST.Term
parseMatchClauses z0 = AST.TermMatch $ go firstClause otherClauses
  where
    go :: Linear -> [Linear] -> [([AST.Destructuring], AST.Term)]
    go lhs [] | Z.isDone $ execState eatWhitespace lhs = []
    go _ [] = error "incomplete match clause"
    go lhs (clause:clauses) = (evalState (exhaustively parseDestructuring) lhs, term) : go nextLHS clauses
      where
        (rhs, nextLHS) = rtl (breakWhen (\t -> kind t == EOL || content t == ",")) clause
        term = parseTerm $ fromMaybe (error "match clauses must be separated by newline or comma") rhs
    (firstClause NE.:| otherClauses) = splitClauses "=" z0

parseMatch :: ParseState AST.Term
parseMatch = eatWhitespace >> gets Z.peek >>= \case
  (Just (LinBraces l)) -> do
    modify Z.eatOne
    pure $ parseMatchClauses $ Z.start l
  _ -> do
    term <- gets parseMatchClauses
    put $ Z.start []
    pure term
