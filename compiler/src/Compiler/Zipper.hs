{-# LANGUAGE DeriveFunctor #-}

module Compiler.Zipper (Zipper(..), todo, done, start, restart, rewind, peek, peekl, left, right, eat, eatOne, Compiler.Zipper.drop, isDone, match, matchCond, filterMaybe) where

import Compiler.Tokenizer (mapFirst)

data Zipper a = Zipper [a] [a] deriving (Eq, Show, Functor)

start :: [a] -> Zipper a
start = Zipper []

restart :: Zipper a -> Zipper a
restart (Zipper _ ft) = Zipper [] ft

rewind :: Zipper a -> Zipper a
rewind (Zipper (x:xs) ft) = rewind $ Zipper xs (x:ft)
rewind z@(Zipper [] ft) = z

todo :: Zipper a -> [a]
todo (Zipper _ a) = a

done :: Zipper a -> [a]
done (Zipper a _) = a

peek :: Zipper a -> Maybe a
peek (Zipper _ (x:_)) = Just x
peek _                = Nothing

peekl :: Zipper a -> Maybe a
peekl (Zipper (x:_) _) = Just x
peekl _                = Nothing

right :: Zipper a -> Maybe (a, Zipper a)
right (Zipper bt (x:xs)) = Just (x, Zipper (x:bt) xs)
right _                  = Nothing

left :: Zipper a -> Maybe (a, Zipper a)
left (Zipper (x:xs) ft) = Just (x, Zipper xs $ x:ft)
left _                  = Nothing

eatOne :: Zipper a -> Zipper a
eatOne z = maybe z snd $ right z

eat :: (a -> Bool) -> Zipper a -> Zipper a
eat predicate z = maybe z (\(x, zr) -> if predicate x
  then eat predicate zr
  else z) $ right z

drop :: (a -> Bool) -> Zipper a -> Zipper a
drop predicate z@(Zipper bt (x:ft)) = if predicate x
  then Compiler.Zipper.drop predicate $ Zipper bt ft
  else z
drop _ z = z

isDone :: Zipper a -> Bool
isDone (Zipper _ ft) = null ft

matchLoop :: (a -> Maybe b) -> ([b], Zipper a) -> ([b], Zipper a)
matchLoop f m@(acc, Zipper bt (a:xs)) = case f a of
  Just b -> matchLoop f (b:acc, Zipper (a:bt) xs)
  Nothing -> m
matchLoop _ m = m

match :: (a -> Maybe b) -> Zipper a -> ([b], Zipper a)
match f z = mapFirst reverse $ matchLoop f ([], z)

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f a = if f a then Just a else Nothing

matchCond :: (a -> Bool) -> Zipper a -> ([a], Zipper a)
matchCond f z = mapFirst reverse $ matchLoop (filterMaybe f) ([], z)
