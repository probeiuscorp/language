{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Compiler.Linearize (linearize, Linear, Linearized, Linearization, GLinearized(..)) where

import Compiler.Tokenize
import qualified Compiler.Zipper as Z
import qualified Control.Monad.State as St

type Tokens = Z.Zipper Token

type Linearization = GLinearization Token
type Linearized = GLinearized Token
type Linear = Z.Zipper Linearized
type GLinearization a = [GLinearized a]
-- | All (GLinearization a) positions contain reversed lists except for the
-- first position of LinFunction.
data GLinearized a
  = LinFunction (GLinearization a) (GLinearization a)
  | LinWhere (GLinearization a) [GLinearization a]
  | LinMultilineOperator String [GLinearization a]
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
  | con == "where" -> let (clauses, zrr) = St.runState offsides zr in
    (pure $ LinWhere (reverse l) (linearize <$> clauses), zrr)
  | kind t == SymbolIdentifier, Just operator <- startsWith '`' con ->
    let (clauses, zrr) = St.runState offsides zr in
      (LinMultilineOperator operator (linearize <$> clauses) : l, zrr)
  | otherwise -> go zr $ LinToken t : l
  where
    go = linearizeOrClose p handleEmpty
    startsWith :: Eq a => a -> [a] -> Maybe [a]
    startsWith _ [] = Nothing
    startsWith a (x:xs) = if a == x then Just xs else Nothing
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

offsides :: St.State Tokens [Tokens]
offsides = goNew Nothing
  where
    goNew s = St.modify (Z.eat $ (/= EOL) . kind) *> go s
    go :: Maybe Int -> St.State Tokens [Tokens]
    go runningCol = do
      St.modify $ Z.eat isWhitespace
      St.gets Z.right >>= \case
        (Just (t, zr)) -> let col = colno $ posRangeStart $ posRange t in
          case maybe EQ (compare col) runningCol of
            -- Continue collecting
            GT -> St.put zr *> go runningCol
            -- Collect in new
            EQ -> do
              x <- St.gets $ Z.start . reverse . Z.done
              St.modify Z.restart
              xs <- goNew $ Just col
              pure $ case runningCol of
                Nothing -> xs  -- no clause yet, should not record
                Just _ -> x : xs
            LT -> St.put zr *> stop
        _ -> stop
      where
        stop = do
          x <- St.gets $ Z.start . reverse . Z.done
          pure $ case runningCol of
            Nothing -> []
            Just _ -> [x]
