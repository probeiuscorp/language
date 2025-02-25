module Compiler.Parse where

import Compiler.Tokenize
import qualified Compiler.AST as AST
import qualified Compiler.Zipper as Z
import Compiler.Linearize (Linear, GLinearized (..), linearize)
import Compiler.ParseInfix (parseInfix)
import Control.Monad (guard, msum, MonadPlus (mzero))
import Control.Monad.State (State, MonadState (state, get, put), gets, evalState, modify, runState, StateT (StateT, runStateT), execState, evalStateT)
import Data.Functor ((<&>), ($>))
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (Bifunctor(first, bimap, second))
import Compiler.Zipper (filterMaybe)

type Tokens = Z.Zipper Token

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) f = (fmap f <$>)
infixl 4 <<$>>

splitDeclarations :: Tokens -> [[Token]]
splitDeclarations z = filter (not . all isWhitespace) $ case Z.eatOne $ Z.eat ((/= EOL) . kind) z of
  (Z.Zipper bt (ch:xs)) -> if doesDeclarationContinue ch
    then splitDeclarations $ Z.Zipper (ch:bt) xs
    else reverse bt : splitDeclarations (Z.Zipper [ch] xs)
  (Z.Zipper bt _) -> pure $ reverse bt

doesDeclarationContinue :: Token -> Bool
doesDeclarationContinue t = con == ")" || con == "}" || con == "]" || kind t == InlineWhitespace
  where con = content t

parseDeclaration :: State Tokens (Maybe AST.TopLevelDeclaration)
parseDeclaration = do
  eatWhitespaceTokens
  gets Z.isDone >>= \case
    True -> pure Nothing
    False -> do
      maybeBinding <- gets . evalStateT $ msum [parseBindingDeclaration, parseDataDeclaration, parseInfixDeclaration]
      specialDeclaration <- parseSpecialDeclaration
      pure . Just $ fromMaybe specialDeclaration maybeBinding
  where
    parseSpecialDeclaration = do
      token <- right
      eatWhitespaceTokens
      case content token of
        "export" -> undefined
        "import" -> parseImportDeclaration
        _ -> undefined

is :: String -> Token -> Bool
is str = (== str) . content
eatIsExported :: StateT Tokens Maybe Bool
eatIsExported = state $ Z.eatIf $ is "export"
eatIdentifier :: StateT Tokens Maybe String
eatIdentifier = catTokens <$> state (Z.match . filterMaybe $ not . isWhitespace)
parseBindingDeclaration :: StateT Tokens Maybe AST.TopLevelDeclaration
parseBindingDeclaration = do
  liftState eatWhitespaceTokens
  isExported <- eatIsExported
  liftState eatWhitespaceTokens
  identifier <- eatIdentifier
  liftState eatWhitespaceTokens
  guard =<< state (Z.eatIf $ is "=")
  term <- gets $ parseTerm . Z.start . linearize
  pure $ AST.ValueDeclaration (AST.DeclarationModule
    { AST.identifier = identifier
    , AST.isExported = isExported
    }) (Just term) Nothing

parseDataDeclaration :: StateT Tokens Maybe AST.TopLevelDeclaration
parseDataDeclaration = do
  liftState eatWhitespaceTokens
  isExported <- eatIsExported
  liftState eatWhitespaceTokens
  guard =<< state (Z.eatIf $ is "data")
  liftState eatWhitespaceTokens
  identifier <- eatIdentifier
  liftState eatWhitespaceTokens
  hasBody <- state $ Z.eatIf $ is "="
  AST.DataDeclaration (AST.DeclarationModule identifier isExported) <$> if hasBody
    then gets $ Just . parseTerm . Z.start . linearize
    else pure Nothing

type ParseAttempt = StateT Tokens Maybe AST.ImportListing
liftState :: State s a -> StateT s Maybe a
liftState f = StateT $ pure . runState f
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
        (LinBraces l:xs) -> guard (onlyWhitespaceLeft $ Z.start xs) $> parseRecordDestructure (Z.start l)
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

parseInfixDeclaration :: StateT Tokens Maybe AST.TopLevelDeclaration
parseInfixDeclaration = do
  associativity <- msum [ifIs "infixl" AST.LeftAssociative, ifIs "infixr" AST.RightAssociative, ifIs "infix" AST.NonAssociative]
  liftState eatWhitespaceTokens
  Token { kind = NumberLiteral numContents } <- liftState right
  liftState eatWhitespaceTokens
  identifier <- content <$> liftState right
  pure $ AST.InfixDeclaration identifier (parsePrecedence numContents) associativity
  where
    ifIs :: String -> a -> StateT Tokens Maybe a
    ifIs keyword value = (guard =<< state (Z.eatIf $ is keyword)) $> value
    parsePrecedence :: NumberContents -> Double
    parsePrecedence (NumberContents { numRadix = radix, numIntegral = integral, numFractional = fractional }) =
      if (all (< 2) <$> fractional) == Just False
        then error "infix precedence fractionals must be binary"
        else fromIntegral (parseIntegral (baseOfRadix radix) integral) + maybe 0 (parseFractional 2) fractional

eatWhitespaceTokens :: State Tokens ()
eatWhitespaceTokens = modify $ Z.eat shouldConsume

type ParseState = State Linear

right :: State (Z.Zipper a) a
right = state $ fromMaybe (error "Unexpected end of input") . Z.right

shouldConsume Token { kind = Comment _ } = True
shouldConsume token = isWhitespace token
eatWhitespace :: ParseState Bool
eatWhitespace = state $ go False
  where
    go hadWhitespace z = fromMaybe (hadWhitespace, z) $ Z.right z >>= \(l, zr) -> case l of
      (LinToken t) | shouldConsume t -> Just $ go True zr
      _ -> Nothing
onlyWhitespaceLeft :: Linear -> Bool
onlyWhitespaceLeft = Z.isDone . execState eatWhitespace

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
  (LinToken (Token { kind = NumberLiteral numContents })) -> ok $ AST.TermNumberLiteral numContents
  (LinToken (Token { kind = StringLiteral str })) -> ok $ AST.TermStringLiteral str
  (LinWhere body clauses) -> ok $ AST.TermWhere (parseTerm $ Z.start body) $ clauses <&> \l ->
    let (Just zBody, zDestruct) = breakWhen (is "=") $ Z.start l in
      (evalState parseDestructuring zDestruct, parseTerm zBody)
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
  (LinToken t) | kind t == LetterIdentifier -> do
    isAsPattern <- state $ Z.eatIf $ \case
      LinToken (Token { content = "@" }) -> True
      _ -> False
    if isAsPattern
      then AST.DestructAs (content t) <$> parseDestructuring
      else pure . AST.DestructBind $ content t
  (LinParens l) -> pure . evalState parseNominal $ Z.start l
  (LinBraces l) -> pure $ parseRecordDestructure $ Z.start l
  _ -> unexpected

parseNominal :: ParseState AST.Destructuring
parseNominal = do
  identifier <- expect matchIdentifier
  destructurings <- exhaustively parseDestructuring
  pure $ AST.DestructNominal identifier destructurings

splitClauses :: String -> Linear -> NE.NonEmpty Linear
splitClauses sep = go
  where
    go z = let (mrest, segment) = breakWhen (is sep) z in
      case mrest of
        Just rest -> segment NE.<| go rest
        Nothing -> pure segment
splitRecordClauses :: String -> Linear -> [(Linear, Maybe Linear)]
splitRecordClauses sep z0 = NE.toList (splitClauses "," z0) >>= parseClause
  where
    parseClause :: Linear -> [(Linear, Maybe Linear)]
    parseClause clause = case breakWhen (is sep) clause of
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
    go lhs [] | onlyWhitespaceLeft lhs = []
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
