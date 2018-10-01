{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.Functor.Compose
import Control.Monad.Free
import Control.Comonad
import Data.List
import Data.Function
import Data.Maybe
import Control.Monad
import Control.Comonad.Cofree
import Control.Applicative
import Data.List.NonEmpty (NonEmpty(..), groupBy1)
import Data.Tree


-- TypeOperators
-- type (:.) f g a = f (g a)


class (Functor f, Functor g) => Push f g where
  part :: f (g a)     -> f (f (g a))
  pver :: f (f (g a)) -> f (g (f a))
  push :: f (g a)     -> f (g (f a))
  push = pver . part

class Pull f g where
  draw :: f (g (f a)) -> f (f (g a))
  bond :: f (f (g a)) -> f (g a)
  pull :: f (g (f a)) -> f (g a)
  pull = bond . draw


instance Functor g => Push IO g where
  part = return
  pver = fmap (fmap return) . join


instance Push [] Maybe where
  part :: [Maybe a] -> [[Maybe a]]
  part = groupBy ((==) `on` isJust)

  pver :: [[Maybe a]] -> [Maybe [a]]
  pver = fmap pver'
    where
      pver' []            = Nothing
      pver' xs@(Just _:_) = Just (fromJust <$> xs)
      pver' _             = Nothing


instance Push NonEmpty Maybe where
  part :: NonEmpty (Maybe a) -> NonEmpty (NonEmpty (Maybe a))
  part = groupBy1 ((==) `on` isJust)

  pver :: NonEmpty (NonEmpty (Maybe a)) -> NonEmpty (Maybe (NonEmpty a))
  pver = fmap pver'
    where
      pver' (Nothing :| _) = Nothing
      pver'  xs            = Just (fromJust <$> xs)


-- base
-- f . g
-- f . g . f
-- f . g . f . g
-- f . g . f . g . f
-- f . g . f . g . f . g

-- g-level (fmap)
-- g
-- g . f
-- g . f . g
-- g . f . g . f
-- g . f . g . f . g

-- f-level (fmap)
--
-- f
-- f . g
-- f . g . f
-- f . g . f . g



-- Cat (type)
-- Dur (steps)
-- Lit (literal tree)
-- Nat (inhabitance type)

-- Cat :: Type -> Expr Cat
-- Dur :: Duration -> Expr Dur

-- X (t :: Expr Cat) (s :: Expr Dur) (l :: Expr Lit) (n :: Expr Nat)


-- X :: (t -> Expr Cat) -> (s -> Expr Dur) -> (

-- Expr a where
--   Cat :: _ -> Expr Cat
--   Dur :: _ -> Expr (a : Cat) -> Expr (a : Dur)
--   Lit :: _ -> Expr (a : Dur) -> Expr (a : Lit)
--   Nat :: _ -> Expr (a : Lit) -> Expr (a : Nat)



-- (t : Cat) (s : Dur) (l : Lit) (n : Nat)

infixr 1 :~
newtype (:~) a b = Ty { getTy :: a }

data Cat a where
  Obj :: a -> Cat (a :~ Cat a)
  Mor :: (a -> b) -> Cat ((a -> b) :~ (Cat a -> Cat b))
  Comp :: Cat ((b -> c) :~ (Cat b -> Cat c)) -> Cat ((a -> b) :~ (Cat a -> Cat b)) -> Cat ((a -> c) :~ (Cat a -> Cat c))
  Id :: Cat ((a -> a) :~ (Cat a -> Cat a))



-- fib : Nat -> Nat
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n-1) + fib0 (n-2)

-- fib : Nat -> Nat
fib1 n | n == 0 = 0
       | n == 1 = 1
       | 2 <= n = fib1 (n-1) + fib1 (n-2)

-- | fib1 is a source transformation of fib0, each having exponential complexity in n when interpreted naively,
-- but they're the standard definition of the fibonacci numbers.
--
-- Compare the timing of fib0 with fib2:
--
-- @
-- λ> fib0 <$> [0..30]
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]
-- (8.97 secs, 2,413,020,008 bytes)
--
-- λ> fib2 <$> [0..30]
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]
-- (0.01 secs, 628,816 bytes)
-- @
--
--
-- In fact, fib2 stays fast up to pretty large values:
--
-- @
-- λ> length $ fib2 <$> [0..10^8]
-- 100000001
-- (3.52 secs, 16,000,374,552 bytes)
--
-- λ> length $ fib2 <$> [0..10^9]
-- 1000000001
-- (35.85 secs, 160,000,375,376 bytes)
-- @
--
-- Suggesting that it's roughly @O(n)@ as we'd expect.
fib2 = loop 0 1
  where
    loop x _ 0 = x
    loop x y n = loop y (x+y) (n-1)


fib3 n = (\(_,_,_,w)->w)$foldr matMult (1, 0, 0, 1) $ zipWith (\m y -> if y == 0 then (1, 0, 0, 1) else m) (iterate (join matMult) fib3Mat) (fmap (flip mod 2) . takeWhile (0 <=) . iterate (flip div 2) $ n)
  where
    fib3Mat = (1, 1, 1, 0)

    matMult (a, b, c, d) (e, f, g, h) = (a * e + b * g, a * f + b * h, c * e + d * g, c * f + d * h)

collatz0 0 = 0
collatz0 1 = 1
collatz0 n | even n = 1 + collatz0 (div n 2)
           | True   = 1 + collatz0 (3*n+1)


collatz1 0 = 0
collatz1 1 = 1
collatz1 n = case mod n 2 of
               0 -> 1 + collatz1 (div n 2)
               _ -> 2 + collatz1 (div (3*n+1) 2)


collatz2 0 = 0
collatz2 1 = 1
collatz2 n = case divMod n 4 of
               (dv, 0) -> 2 + collatz2 dv
               (dv, 1) -> 3 + collatz2 (3 * dv + 1)
               (dv, 2) -> 3 + collatz2 (3 * dv + 2)
               (dv, 3) -> 3 + collatz2 (9 * dv + 8)


-- n = 4 * div n 4 + mod n 4

-- mod = 0, 2 -> even

-- 0 -> div n 4 --
-- 1 -> 3*div n 4 + 1 -- 3 * (4*div n 4 + 1) + 1 = 3 * 4 * div n 4 + 3 + 1 = 4 * (3*div n 4 + 1) => 3 * div n 4 + 1
-- 2 -> 3*div n 4 + 2 -- div (4*div n 4 + 2) 2 = 2*div n 4 + 1 => 3 * (2*div n 4 + 1) + 1 = 3 * 2 * div n 4 + 3 + 1 = 2 * 3 * div n 4 + 2 * 2 => 3 * div n 4 + 2
-- 3 -> -- 3*(4*div n 4 + 3) + 1 = 3 * 4 * div n 4 + 9 + 1 = 3 * 4 * div n 4 + 10 = 6 * div n 4 + 5 => 3 * (6 * div n 4 + 5) + 1 = 2 * 9 * div n 4 + 16 => 9 * div n 4 + 8


-- (fib : Nat -> Nat) (0 : _) = (0 : Nat)
-- (fib : Nat -> Nat) (1 : _) = (1 : Nat)
-- (fib : Nat -> Nat) (n : _) = ((+) : _) ((fib : Nat -> Nat) (((-) : _) (n : _) (1 : _))) ((fib : Nat -> Nat) (((-) : _) (n : _) (2 : _)))
-- (fib : Nat -> Nat) (n : _) = ((+) : _ -> _ -> _) ((fib : Nat -> Nat) (((-) : _ -> _ -> _) (n : _) (1 : _))) ((fib : Nat -> Nat) (((-) : _ -> _ -> _) (n : _) (2 : _)))
-- (fib : Nat -> Nat) (n : _) = ((+) : _ -> _ -> Nat) ((fib : Nat -> Nat) (((-) : _ -> _ -> Nat) (n : _) (1 : _))) ((fib : Nat -> Nat) (((-) : _ -> _ -> Nat) (n : _) (2 : _)))
-- (fib : Nat -> Nat) (n : Nat) = ((+) : _ -> _ -> Nat) ((fib : Nat -> Nat) (((-) : _ -> _ -> Nat) (n : _) (1 : _))) ((fib : Nat -> Nat) (((-) : _ -> _ -> Nat) (n : _) (2 : _)))
-- (fib : Nat -> Nat) (n : Nat) = ((+) : Nat -> Nat -> Nat) ((fib : Nat -> Nat) (((-) : Nat -> _ -> Nat) (n : Nat) (1 : _))) ((fib : Nat -> Nat) (((-) : Nat -> _ -> Nat) (n : Nat) (2 : _)))
-- (fib : Nat -> Nat) (n : Nat) = ((+) : Nat -> Nat -> Nat) ((fib : Nat -> Nat) (((-) : Nat -> Nat -> Nat) (n : Nat) (1 : Nat))) ((fib : Nat -> Nat) (((-) : Nat -> Nat -> Nat) (n : Nat) (2 : Nat)))

-- fib : ((-) : Nat -> Nat -> Nat, (+) : Nat -> Nat -> Nat, 0 : Nat, 2 : Nat, (2 <= n) => (n-1) : Nat, (2 <= n) => (n-2) : Nat) => Nat -> Nat
-- 1 #: fib 0 = 0 :# nat
-- 1 #: fib 1 = 1 :# nat
-- (2 * (_ #: (-)) + (_ #: (+)) + (_ #: fib (n-1)) + (_ #: fib (n-2))) #: fib n = fib (n-1) + fib (n-2)


-- fib_inc : Bool
-- fib_inc = forall (n : Nat). 2 <= n => fib n < fib (n+1)
-- fib_inc = all fib_inc [0..]
--   = all fib_inc [0..3] && all fib_inc (n+4) [0..]
--   = all (\n -> fib (n+4) < fib (n+5)) [0..]
--   = all (\n -> 0 < fib (n+3)) [0..]
--   = all (\n -> 0 < fib (n+2) + fib (n+1)) [0..]
--   = all (\n -> 0 < fib (n+1) + fib n + fib (n+1)) [0..]
--   = all (\n -> 0 < fib (n+1) || 0 < fib n) [0..]
--   = all (\n -> 0 < fib (n+1)) [0..]
--   = induce refl (\n -> 0 < fib (n+1) => 0 < fib (n+2))
--   = induce refl (\n -> 0 < fib (n+1) => 0 < fib (n+1) + fib (n+2))
--   = induce refl refl
--   where
--     induce : \p -> p 0 && (forall (n : Nat). p n => p (n+1)) => forall (n : Nat). p n





instance Push Tree Maybe where
  part :: Tree (Maybe a) -> Tree (Tree (Maybe a))
  part = noNothing2 . duplicate

  pver :: Tree (Tree (Maybe a)) -> Tree (Maybe (Tree a))
  pver = fmap sequence


noNothing :: Tree (Maybe a) -> Tree (Maybe a)
noNothing (Node Nothing _) = Node Nothing []
noNothing (Node x xs) = Node x (map noNothing . filter (isJust . extract) $ xs)

noNothing2 :: Tree (Tree (Maybe a)) -> Tree (Tree (Maybe a))
noNothing2 (Node (Node Nothing _) _) = Node (Node Nothing []) []
noNothing2 (Node x xs) = Node (noNothing x) (map noNothing2 . filter (isJust . extract . extract) $ xs)

-- noNothing :: Forest (Maybe a) -> Forest (Maybe a)
-- noNothing = map (forestMap noNothing) . filter (isJust . extract)

forestMap :: (Forest a -> Forest a) -> Tree a -> Tree a
forestMap f (Node x xs) = Node x (f xs)

-- | Recursively filter a Tree by values
filterTree :: (a -> Bool) -> a -> Tree a -> Tree a
filterTree f x0 (Node x xs) | f x       = Node x (filterTree f x0 <$> filter (f . extract) xs)
                            | otherwise = Node x0 []


-- prop_pushListMaybe :: Eq a => [Maybe a] -> Bool
-- prop_pushListMaybe = liftM2 (==) (fmap sequence . duplicate) push




instance Applicative g => Push [] (Free g) where
  part :: [Free g a] -> [[Free g a]]
  part = groupBy ((==) `on` isPure)

  pver :: [[Free g a]] -> [Free g [a]]
  pver = fmap pver'
    where
      pver' [] = Pure []
      pver' xs@(Pure _:_) = Pure ((\(~(Pure x)) -> x) <$> xs)
      pver' xs            = Free (getCompose . sequenceA $ Compose . (\(~(Free x)) -> x) <$> xs)


instance Alternative g => Push [] (Cofree g) where
  part :: [Cofree g a] -> [[Cofree g a]]
  part = return

  pver :: [[Cofree g a]] -> [Cofree g [a]]
  pver = fmap pver'
    where
      pver' x = ((\(y :< _) -> y) <$> x) :< (getCompose . sequenceA . fmap (Compose . (\(_ :< ys) -> ys)) $ x)

-- f (g a) -> f (f (g a))
-- f (a, g a) -> f (f (a, g a))



isPure :: Free f a -> Bool
isPure (Pure _) = True
isPure  _       = False

-- | f'(expr)
quote :: Push f g => (f (f (g a)) -> f (f (g b))) -> f (g a) -> f (g (f b))
quote f expr = (pver . f . part) expr

-- | f`(expr)
unquote :: Pull f g => (f (f (g a)) -> f (f (g b))) -> f (g (f a)) -> f (g b)
unquote f expr = (bond . f . draw) expr




-- instance Push f g => Push (Cofree f) g where
--   part :: Cofree f (g a) -> Cofree f (Cofree f (g a))
--   part = _ (undefined :: f (g a) -> f (f (g a)))
--   pver :: Cofree f (Cofree f (g a)) -> Cofree f (g (Cofree f a))
--   pver = _ (undefined :: f (f (g a)) -> f (g (f a)))


-- instance Push f g => Push (Free f) g where
--   part :: Free f (g a) -> Free f (Free f (g a))
--   part = _ (undefined :: f (g a) -> f (f (g a)))
--   pver :: Free f (Free f (g a)) -> Free f (g (Free f a))
--   pver = _ (undefined :: f (f (g a)) -> f (g (f a)))




someFunc :: IO ()
someFunc = putStrLn "someFunc"
