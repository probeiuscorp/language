{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Compiler.Linearizer (linearize, Linear, Linearized, Linearization, GLinearized(..)) where

import Compiler.Tokenizer
import qualified Compiler.Zipper as Z

type Tokens = Z.Zipper Token

type Linearization = GLinearization Token
type Linearized = GLinearized Token
type Linear = Z.Zipper Linearized
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
fixOrder (LinFunction params body) = LinFunction (fixOrder <$> params) . reverse $ fixOrder <$> body
fixOrder (LinBrackets body) = LinBrackets . reverse $ fixOrder <$> body
fixOrder (LinBraces body) = LinBraces . reverse $ fixOrder <$> body
fixOrder (LinParens body) = LinParens . reverse $ fixOrder <$> body
fixOrder linearized = linearized

-- | Linearizing sorts out productions which require seeking. Mostly it is for
-- handling function expressions, but while it's here, it may as well handle
-- closing pairs too.
linearize :: Tokens -> Linearization
linearize z0 = reverse $ fixOrder <$> fst (linearizeOrClose (const Continue) id z0 [])

data CloseAction = Continue | Eat | Keep deriving (Eq, Show)
linearizeOrClose :: WithConfig (Tokens -> Continue)
linearizeOrClose p handleEmpty z l = maybe (handleEmpty (l, z)) (\(t, zr) ->
  case p t of
    Continue -> matchHead p handleEmpty (t, zr) l
    Eat -> (l, zr)
    Keep -> (l, z)
  ) $ Z.right z

type Continue = Linearization -> (Linearization, Tokens)
type WithConfig a = (Token -> CloseAction) -> ((Linearization, Tokens) -> (Linearization, Tokens)) -> a
matchHead :: WithConfig ((Token, Tokens) -> Continue)
matchHead p handleEmpty (t, zr) l = let con = content t in if
  | con == "." && matchesFunction zr -> fn
  | con == "(" -> pair LinParens ")"
  | con == "{" -> pair LinBraces "}"
  | con == "[" -> pair LinBrackets "]"
  | otherwise -> go zr $ LinToken t : l
  where
    go = linearizeOrClose p handleEmpty
    pairlike construct lbefore (lcontents, tokensAfter) = go tokensAfter $ construct lcontents : lbefore
    tokenIs con t' = if content t' == con then Eat else Continue
    pair construct exitSeq = pairlike construct l $
      linearizeOrClose (tokenIs exitSeq) (const $ error "unclosed group") zr []
    -- Linearizing functions
    (Z.Zipper lparams lpreparams) = Z.eat shouldBeMadeParam $ Z.start l
    pFnBody t'
      | content t' == "," = Keep
      | p t' == Continue  = Continue
      | otherwise = Keep  -- if p wanted to eat the token, then we should keep it so caller can then eat it
    fn = pairlike (LinFunction lparams) lpreparams $
      linearizeOrClose pFnBody id zr []

matchesFunction :: Tokens -> Bool
matchesFunction zr = (isWhitespace <$> (z >>= Z.peekl)) == Just False && (isWhitespace <$> Z.peek zr) == Just True
  where z = snd <$> Z.left zr

shouldBeMadeParam :: Linearized -> Bool
shouldBeMadeParam (LinToken t) = kind t /= SymbolIdentifier
shouldBeMadeParam (LinFunction _ _ ) = False
shouldBeMadeParam _ = True
