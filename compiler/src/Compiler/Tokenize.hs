module Compiler.Tokenize (
  tokenize, Token(..), FilePos(..), PosRange(..), TokenKind(..),
  NumberContents(..), Radix(..), parseIntegral, parseFractional, numScalar, baseOfRadix,
  isWhitespace, catTokens
) where

import qualified Compiler.Zipper as Z
import Data.Char (isDigit, isAlphaNum, isSpace, digitToInt, isHexDigit, isOctDigit)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Control.Monad.State (MonadState (state, put), gets, StateT (runStateT, StateT), State, runState)
import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor(first))
import Control.Applicative (asum)

data Radix = RadixBin | RadixOct | RadixDec | RadixHex
  deriving (Eq, Show, Ord)
data NumberContents = NumberContents
  { numIsPos :: Bool
  , numRadix :: Radix
  , numIntegral :: [Int]
  , numFractional :: Maybe [Int]
  , numExponent :: Maybe Int
  } deriving (Eq, Show, Ord)
parseIntegral :: Int -> [Int] -> Int
parseIntegral base = foldl (\acc n -> acc * base + n) 0
parseFractional :: Int -> [Int] -> Double
parseFractional base = snd . foldl (\(pos, acc) n ->
    (pos + 1, acc + (fromIntegral n / (fromIntegral base ** pos)))
  ) (1, 0)
numScalar :: NumberContents -> Double
numScalar (NumberContents { numIsPos = isPos, numExponent = mExponent, numRadix = radix }) =
  (if isPos then id else negate) $ maybe 1 ((fromIntegral $ baseOfRadix radix) ^^) mExponent
baseOfRadix :: Radix -> Int
baseOfRadix RadixBin = 2
baseOfRadix RadixOct = 8
baseOfRadix RadixDec = 10
baseOfRadix RadixHex = 16

data CommentKind = LineComment | InlineComment deriving (Eq, Show, Ord)
data TokenKind
  = InlineWhitespace | EOL
  | LetterIdentifier | SymbolIdentifier
  | StringLiteral String
  | NumberLiteral NumberContents
  | Comment CommentKind
  deriving (Eq, Show, Ord)
data FilePos = FilePos { lineno :: Int, colno :: Int }
  deriving (Eq, Ord, Show)
data PosRange = PosRange { posRangeStart :: FilePos, posRangeEnd :: FilePos }
  deriving (Eq, Ord, Show)
data Token = Token
  { kind :: TokenKind
  , content :: String
  , posRange :: PosRange
  } deriving (Eq, Show, Ord)
type TokenMatch = (TokenKind, String)

isWhitespace :: Token -> Bool
isWhitespace token = kind token == InlineWhitespace || kind token == EOL

catTokens :: [Token] -> String
catTokens = foldr ((++) . content) mempty

matchSpan :: TokenKind -> (Char -> Bool) -> NonEmpty Char -> (TokenMatch, String)
matchSpan matchKind isMatch str = first (matchKind,) $ NE.span isMatch str

shouldTokenizeAlone :: Char -> Bool
shouldTokenizeAlone ch =
  ch == '(' || ch == ')' ||
  ch == '[' || ch == ']' ||
  ch == '{' || ch == '}'

isSymbol :: Char -> Bool
isSymbol ch = not $ isDigit ch || isAlphaNum ch || isSpace ch || shouldTokenizeAlone ch

goTokenize :: FilePos -> String -> [Token]
goTokenize _ [] = []
goTokenize currentPos (ch:rest) = Token tokenKind tokenContent (PosRange currentPos nextPos) : goTokenize nextPos nextSource
  where
    ((tokenKind, tokenContent), nextSource) = fromMaybe tokenMatch blindMatches
    str = ch:|rest
    nextPos :: FilePos
    nextPos = flip (`foldl` currentPos) tokenContent $ \x y -> if y == '\n'
      then FilePos { lineno = lineno x + 1, colno = 1 }
      else FilePos { lineno = lineno x, colno = colno x + 1 }
    blindMatches = runStateT (asum [matchNumberLiteral, matchInlineComment]) (Z.start (ch:rest)) <&> \(numKind, Z.Zipper consumed zr) ->
      ((numKind, reverse consumed), zr)
    tokenMatch :: (TokenMatch, String)
    tokenMatch
      | isAlphaNum ch = matchSpan LetterIdentifier isAlphaNum str
      | '\n' == ch    = ((EOL, pure ch), rest)
      | isSpace ch    = matchSpan InlineWhitespace isInlineWhitespaceCh str
      | '"' == ch     = matchStringLiteral rest & first (\parsed -> (
        StringLiteral parsed,
        parsed -- FIXME: this should contain the source of the string
      ))
      | shouldTokenizeAlone ch = ((SymbolIdentifier, pure ch), rest)
      | otherwise = if snd token == "--"
        then first ((Comment LineComment,) . ("--" ++)) $ span (/= '\n') after
        else match
        where match@(token, after) = matchSpan SymbolIdentifier isSymbol str
    isInlineWhitespaceCh :: Char -> Bool
    isInlineWhitespaceCh '\n' = False
    isInlineWhitespaceCh ch = isSpace ch

tokenize :: String -> [Token]
tokenize = goTokenize $ FilePos 1 1

matchInlineComment :: StateT (Z.Zipper Char) Maybe TokenKind
matchInlineComment = matchStart *> StateT (Just . runState (go []))
  where
    go :: [()] -> State (Z.Zipper Char) TokenKind
    go k = do
      matchIsEnd <- gets $ runStateT $ asum [False <$ matchStart, True <$ matchEnd]
      maybe (pure ()) (put . snd) matchIsEnd
      case fst <$> matchIsEnd of
        Just False -> go $ () : k
        Just True -> case k of
          [] -> pure $ Comment InlineComment
          (_:ks) -> go ks
        Nothing -> gets Z.right >>= \case
          Just (_, zr) -> put zr *> go k
          Nothing -> pure $ Comment InlineComment
    matchStart = eatenIs '{' *> eatenIs '-'
    matchEnd = eatenIs '-' *> eatenIs '}'
    eatenIs :: Char -> StateT (Z.Zipper Char) Maybe ()
    eatenIs ch = StateT Z.right >>= guard . (ch ==)

matchStringLiteral :: String -> (String, String)
matchStringLiteral ('\\':xs) = matchEscapeSequence xs
matchStringLiteral ('"':xs)  = ([], xs)
matchStringLiteral (x:xs)    = matchStringLiteral xs & first (x:)
matchStringLiteral []        = error "Unterminated string"

matchEscapeSequence :: String -> (String, String)
matchEscapeSequence ('u':xs) = undefined
matchEscapeSequence (x:xs)   = (pure $ fromMaybe (error $ "unknown escape sequence: " ++ [x]) $ matchEscape x, xs)
matchEscapeSequence []       = ([], [])

matchEscape :: Char -> Maybe Char
matchEscape '\\' = Just '\\'
matchEscape 'n' = Just '\n'
matchEscape 't' = Just '\t'
matchEscape 'r' = Just '\r'
matchEscape 'c' = Just '\f'
matchEscape 'a' = Just '\a'
matchEscape 'b' = Just '\b'
matchEscape 'v' = Just '\v'
matchEscape _   = Nothing

data ExplicitSign = Pos | Neg | NoMatch deriving (Eq, Show)
type NumLitState = StateT (Z.Zipper Char) Maybe
matchNumberLiteral :: NumLitState TokenKind
matchNumberLiteral = do
  explicitSign <- state $ \z -> case Z.right z of
    Just ('+', zr) -> (Pos, zr)
    Just ('-', zr) -> (Neg, zr)
    _ -> (NoMatch, z)
  pointingAtDigit <- gets $ \z -> case Z.peek z of
    Just ch -> isDigit ch
    _ -> False
  guard pointingAtDigit
  (radix, isRadixDigit) <- state $ \case
    (Z.Zipper bt ('0':'b':ft)) -> ((RadixBin, isBinDigit), Z.Zipper ('b':'0':bt) ft)
    (Z.Zipper bt ('0':'o':ft)) -> ((RadixOct, isOctDigit), Z.Zipper ('o':'0':bt) ft)
    (Z.Zipper bt ('0':'x':ft)) -> ((RadixHex, isHexDigit), Z.Zipper ('x':'0':bt) ft)
    z -> ((RadixDec, isDigit), z)
  integral <- state $ Z.matchCond isRadixDigit
  fractionals <- peekThen (== '.') $ state $ Z.matchCond isRadixDigit
  exponents <- if radix /= RadixHex
    then peekThen (\ch -> ch == 'e' || ch == 'E') $ liftA2 ($)
      (StateT $ \z -> Z.right z <&> \(ch, zr) -> if ch == '-'
        then ((ch:), zr)  -- eat and prepend to matched
        else (id, if ch == '+' then zr else z)  -- don't prepend '+' because `read` does not like it, but do eat '+'
      )
      (state $ Z.matchCond isDigit)
    else pure Nothing
  pure . NumberLiteral $ NumberContents {
    numIsPos = explicitSign /= Neg,
    numRadix = radix,
    numIntegral = digitToInt <$> integral,
    numFractional = fmap digitToInt <$> fractionals,
    numExponent = read <$> exponents
  }
  where
    isBinDigit ch = ch == '0' || ch == '1'
    peekThen :: (Char -> Bool) -> NumLitState a -> NumLitState (Maybe a)
    peekThen p run = gets Z.right >>= \case
      (Just (ch, zr)) | p ch -> put zr >> Just <$> run
      _ -> pure Nothing
