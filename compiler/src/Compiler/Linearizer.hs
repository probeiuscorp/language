{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Compiler.Linearizer (linearize, Linearized, GLinearized(..)) where

import Compiler.Tokenizer
import qualified Compiler.Zipper as Z

type Tokens = Z.Zipper Token

type Linearization = GLinearization Token
type Linearized = GLinearized Token
type GLinearization a = [GLinearized a]
-- | All (GLinearization a) positions contain reversed lists except for the
-- first position of LinFunction.
data GLinearized a
  = LinFunction (GLinearization a) (GLinearization a)
  | LinBrackets (GLinearization a)
  | LinBraces (GLinearization a)
  | LinParens (GLinearization a)
  | LinToken a
  deriving (Eq, Show, Functor)

fixOrder :: GLinearized a -> GLinearized a
fixOrder (LinFunction params body) = LinFunction params $ reverse body
fixOrder (LinBrackets body) = LinBrackets $ reverse body
fixOrder (LinBraces body) = LinBraces $ reverse body
fixOrder (LinParens body) = LinParens $ reverse body
fixOrder linearized = linearized

-- | Linearizing sorts out productions which require seeking. Mostly it is for
-- handling function expressions, but while it's here, it may as well handle
-- closing pairs too.
linearize :: Tokens -> Linearization
linearize z = reverse $ fixOrder <$> fst (linearizeL z [])

linearizeL :: Tokens -> Continue
linearizeL z l = maybe (l, Z.start []) (flip (matchHead linearizeL) l) $ Z.right z

linearizeOrClose :: String -> Tokens -> Continue
linearizeOrClose exitSeq z l = maybe (error "unclosed group") (\(t, zr) ->
  if content t == exitSeq
    then (l, zr)
    else matchHead (linearizeOrClose exitSeq) (t, zr) l
  ) $ Z.right z

type Continue = Linearization -> (Linearization, Tokens)
matchHead :: (Tokens -> Continue) -> (Token, Tokens) -> Continue
matchHead continue (t, zr) l = let con = content t in if
  | (con == "." && matchesFunction zr) -> (linearizeFunction, zrr_f)
  | con == "(" -> pair LinParens ")"
  | con == "{" -> pair LinBraces "}"
  | con == "[" -> pair LinBrackets "]"
  | otherwise -> continue zr (LinToken t : l)
  where
    pair construct exitSeq =
      let (linearizedContents, tokensAfter) = linearizeOrClose exitSeq zr []
      in continue tokensAfter (construct linearizedContents : l)
    -- Linearizing functions
    (Z.Zipper lParams lPreParams) = Z.eat shouldBeMadeParam $ Z.start l
    (lBody, zrr_f) = continue zr []  -- Provide empty Linearization so `lBody` doesn't contain params
    linearizeFunction = LinFunction lParams lBody : lPreParams

matchesFunction :: Tokens -> Bool
matchesFunction zr = (isWhitespace <$> (z >>= Z.peekl)) == Just False && (isWhitespace <$> Z.peek zr) == Just True
  where z = snd <$> Z.left zr

shouldBeMadeParam :: Linearized -> Bool
shouldBeMadeParam (LinToken t) = kind t /= SymbolIdentifier
shouldBeMadeParam (LinFunction _ _ ) = False
shouldBeMadeParam _ = True
