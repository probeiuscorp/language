module Compiler.Tokenizer (tokenize, Token(Token), TokenKind(..), content, kind) where
import Data.Char (isDigit, isAlphaNum, isSpace, isAlpha)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NE

data TokenKind =
    InlineWhitespace | EOL |
    LetterIdentifier | SymbolIdentifier |
    NumberLiteral deriving (Eq, Show)
data Token = Token
    { kind :: TokenKind
    , content :: String
    } deriving (Eq, Show)

matchSpan :: TokenKind -> (Char -> Bool) -> NonEmpty Char -> (Token, String)
matchSpan kind isMatch str =
    let (content, rest) = NE.span isMatch str in (Token {
        kind = kind,
        content = content
    }, rest)

isSymbol :: Char -> Bool
isSymbol ch = not $ isDigit ch || isAlphaNum ch || isSpace ch

readToken :: NonEmpty Char -> (Token, String)
readToken str@(ch:|rest)
    | isDigit ch    = matchSpan NumberLiteral isDigit str
    | isAlphaNum ch = matchSpan LetterIdentifier isAlphaNum str
    | '\n' == ch    = (Token {
        kind = EOL,
        content = pure ch
    }, rest)
    | isSpace ch    = matchSpan InlineWhitespace isSpace str
    | otherwise     = matchSpan SymbolIdentifier isSymbol str

tokenize :: String -> [Token]
tokenize str = case nonEmpty str of
    Just a -> let (token, rest) = readToken a in
        token:tokenize rest
    Nothing -> []
