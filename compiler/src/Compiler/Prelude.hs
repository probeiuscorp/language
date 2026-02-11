-- | I'd rather import common names once and never again.
-- This cuts down on diff churn and being annoyed by having to import things.

module Compiler.Prelude (
  module Compiler.Prelude,
  on, bimap, first, second,
  asum, void, guard, join, (<|>),
  fromMaybe, toList,
  ($>), (&), (<&>),
) where

import Data.Function (on, (&))
import Data.Functor ((<&>), ($>))
import Data.Bifunctor (bimap, first, second)
import Control.Applicative (asum, (<|>))
import Control.Monad (void, guard, join)
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable (toList))

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f a = if f a then Just a else Nothing

($:) :: (a -> b) -> a -> b
($:) = ($)
infixr 6 $:

($$$) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
($$$) = fmap . fmap
