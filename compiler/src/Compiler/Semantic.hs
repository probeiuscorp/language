module Compiler.Semantic where

import qualified Compiler.AST as AST
import qualified Compiler.Tokenize as Tok
import qualified Data.Set as Set
import Control.Monad.Reader (Reader, MonadReader (ask, local), runReader, ReaderT (runReaderT))
import Control.Monad.Writer (tell, Writer, runWriter)
import Data.Function (on)

tryIntFromDouble :: Double -> Maybe Int
tryIntFromDouble double = if ceiled == floor double
  then Just ceiled
  else Nothing
  where ceiled = ceiling double

type Semant a = ReaderT AST.VarSet (Writer [AST.ParseError]) (Maybe a)
tellErr :: AST.ParseError -> Semant AST.Expression
tellErr err = Nothing <$ tell (pure err)
semanticValue :: AST.VarSet -> AST.Term -> Either [AST.ParseError] AST.Expression
semanticValue knownVars term = case runWriter $ runReaderT (go term) knownVars of
  (Just expr, _) -> Right expr
  (_, errs) -> Left errs
  where
    ok = pure . pure
    go :: AST.Term -> Semant AST.Expression
    go fn@(AST.TermFunction (d:ds) body) = do
      bodyExpr <- local (mappend $ collectBindings d) $ go $ AST.TermFunction ds body
      pure $ AST.ExprFunction (collectFreeVariables fn) d <$> bodyExpr
    go (AST.TermFunction [] body) = go body
    go (AST.TermApplication target argument) =
      (liftA2 (liftA2 AST.ExprApplication) `on` go) target argument
    go (AST.TermIdentifier ident) = do
      scope <- ask
      if Set.member ident scope
        then ok $ AST.ExprIdentifier ident
        else tellErr $ AST.ErrUnknownIdentifier ident
    go (AST.TermNumberLiteral numContents@(Tok.NumberContents
      { Tok.numIntegral = integral
      , Tok.numFractional = mFractional
      , Tok.numRadix = radix
      })) = ok $ case (mFractional, mScalar) of
        (Nothing, Right scalar) -> AST.ExprIntegral $ scalar * integralPart
        _ -> AST.ExprDouble $ either id fromIntegral mScalar * (fromIntegral integralPart + maybe 0 (Tok.parseFractional base) mFractional)
      where
        base = Tok.baseOfRadix radix
        dblScalar = Tok.numScalar numContents
        mScalar = maybe (Left dblScalar) Right $ tryIntFromDouble dblScalar
        integralPart = Tok.parseIntegral base integral
    go (AST.TermMatch clauses) = fmap AST.ExprMatch . sequence <$> traverse visitClause clauses
      where
        visitClause ([destruct], term') = fmap (destruct, ) <$> local (<> collectBindings destruct) (go term')
        visitClause _ = undefined
    go _ = undefined

collectFreeVariables :: AST.Term -> AST.VarSet
collectFreeVariables term = runReader (go term) mempty
  where
    go :: AST.Term -> Reader AST.VarSet AST.VarSet
    go (AST.TermIdentifier ident) = do
      boundVars <- ask
      pure $ if Set.member ident boundVars
        then mempty
        else Set.singleton ident
    go (AST.TermFunction destructs body) = local (mappend $ foldMap collectBindings destructs) $ go body
    go (AST.TermApplication lhs rhs) = (liftA2 mappend `on` go) lhs rhs
    go _ = pure mempty

collectBindings :: AST.Destructuring -> AST.VarSet
collectBindings (AST.DestructBind ident) = Set.singleton ident
collectBindings (AST.DestructAs ident destruct) = Set.singleton ident <> collectBindings destruct
collectBindings (AST.DestructNominal _ destructs) = foldMap collectBindings destructs
collectBindings (AST.DestructRecord rows) = foldMap (\case
    (_, Just destruct) -> collectBindings destruct
    (ident, Nothing) -> Set.singleton ident
  ) rows
