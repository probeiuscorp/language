{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Compiler.Linearizer (linearize, Linearized, GLinearized(..)) where

import Compiler.Tokenizer
import qualified Compiler.Zipper as Z

type Tokens = Z.Zipper Token

type Linearization = GLinearization Token
type Linearized = GLinearized Token
type GLinearization a = [GLinearized a]
data GLinearized a
  = LinFunction (GLinearization a) (GLinearization a)
  | LinBrackets (GLinearization a)
  | LinBraces (GLinearization a)
  | LinParens (GLinearization a)
  | LinTuple [GLinearization a]
  | LinToken a
  deriving (Eq, Show, Functor)

linearize :: Tokens -> Linearization
linearize z = fst $ linearizeL z []

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
  | con == "(" -> pair LinParens ")"
  | con == "{" -> pair LinBraces "}"
  | con == "[" -> pair LinBrackets "]"
  | otherwise -> continue zr (LinToken t : l)
  where
    pair construct exitSeq =
      let (linearizedContents, tokensAfter) = linearizeOrClose exitSeq zr []
      in continue tokensAfter (construct linearizedContents : l)
