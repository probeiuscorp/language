module Compiler.Tokenize (tokenize, Token(Token), TokenKind(..), content, kind, isWhitespace, catTokens) where
import Data.Char (isDigit, isAlphaNum, isSpace)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Bifunctor (Bifunctor(first))

data TokenKind
  = InlineWhitespace | EOL
  | LetterIdentifier | SymbolIdentifier
  | NumberLiteral | StringLiteral String
  deriving (Eq, Show)
data Token = Token
  { kind :: TokenKind
  , content :: String
  } deriving (Eq, Show)

isWhitespace :: Token -> Bool
isWhitespace token = kind token == InlineWhitespace || kind token == EOL

catTokens :: [Token] -> String
catTokens = foldr ((++) . content) mempty

matchSpan :: TokenKind -> (Char -> Bool) -> NonEmpty Char -> (Token, String)
matchSpan kind isMatch str = NE.span isMatch str & first (\content -> Token {
  kind = kind,
  content = content
})

shouldTokenizeAlone :: Char -> Bool
shouldTokenizeAlone ch =
  ch == '(' || ch == ')' ||
  ch == '[' || ch == ']' ||
  ch == '{' || ch == '}'

isSymbol :: Char -> Bool
isSymbol ch = not $ isDigit ch || isAlphaNum ch || isSpace ch || shouldTokenizeAlone ch

readToken :: NonEmpty Char -> (Token, String)
readToken str@(ch:|rest)
  | isDigit ch    = matchSpan NumberLiteral isDigit str
  | isAlphaNum ch = matchSpan LetterIdentifier isAlphaNum str
  | '\n' == ch    = (Token {
    kind = EOL,
    content = pure ch
  }, rest)
  | isSpace ch    = matchSpan InlineWhitespace isInlineWhitespaceCh str
  | '"' == ch     = matchStringLiteral rest & first (\parsed -> Token {
    kind = StringLiteral parsed,
    content = parsed -- FIXME: this should contain the source of the string
  })
  | shouldTokenizeAlone ch = (Token {
    kind = SymbolIdentifier,
    content = pure ch
  }, rest)
  | otherwise     = matchSpan SymbolIdentifier isSymbol str
  where
    isInlineWhitespaceCh :: Char -> Bool
    isInlineWhitespaceCh '\n' = False
    isInlineWhitespaceCh ch = isSpace ch

tokenize :: String -> [Token]
tokenize str = case nonEmpty str of
  Just a -> let (token, rest) = readToken a in
    token:tokenize rest
  Nothing -> []

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
