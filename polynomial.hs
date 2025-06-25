 module Polynomial (expand, simplify, Polynomial(..)) where

import           GHC.Natural (Natural)
import GHC.TypeLits (Nat)


type Deg = Natural
type Coeff = Integer

data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial
    deriving Show

-- eliminate multiplication
expand :: Polynomial -> Polynomial
expand = expand' 


-- simplified polynom is returned in descending degree
simplify :: Polynomial -> Polynomial
simplify = simplify'


expand' :: Polynomial -> Polynomial
expand' (Mono c d) = Mono c d
expand' (Add f g) = Add (expand' f) (expand' g)
expand' (Mul (Mono c0 d0) (Mono c1 d1)) = Mono (c0*c1) (d0+d1)
expand' (Mul (Add f g) h) = Add (expand' $ Mul f h) (expand' $ Mul g h)  -- right dist
expand' (Mul f (Add g h)) = Add (expand' $ Mul f g) (expand' $ Mul f h)  -- left dist
expand' (Mul f g) = expand' $ Mul (expand' f) (expand' g)


-- simplified polynom is returned in descending degree

simplify' :: Polynomial -> Polynomial
simplify' (Mono c d) = Mono c d
simplify' (Add g h)  = merge' (simplify' g) (simplify' h)
simplify' f          = simplify' $ expand' f


-- Precondition: input is simplified

merge' :: Polynomial -> Polynomial -> Polynomial
merge' (Mono a b) (Mono c d)
    | b > d      = Add (Mono a b) (Mono c d)
    | d > b      = Add (Mono c d) (Mono a b)
    | otherwise  = Mono (a+c) d

merge' (Mono lcf df) g
    | df > dg   = Add (Mono lcf df) g
    | dg > df   = Add (Mono lcg dg) $ merge' (Mono lcf df) gt
    | otherwise = Add (Mono (lcf+lcg) df) gt
  where
    Add (Mono lcg dg) gt = g
merge' f (Mono c d) = merge' (Mono c d) f

merge' f g
    | df > dg   = Add (Mono lcf df) (merge' ft g)
    | dg > df   = Add (Mono lcg dg) (merge' gt f)
    | otherwise = Add (Mono (lcf+lcg) df) (merge' ft gt)
  where
    Add (Mono lcf df) ft = f
    Add (Mono lcg dg) gt = g

-- Utility function to convert subtraction to add (mainly for parsing)
negatePoly :: Polynomial -> Polynomial 
negatePoly (Mono c d)     = Mono (-c) d
negatePoly (Add f g)      = Add (negatePoly f) (negatePoly g)
negatePoly (Mul f g)      = Mul (negatePoly f) g  -- or Mul f (negatePoly g), both are fine

subPoly :: Polynomial -> Polynomial -> Polynomial
subPoly f g = Add f (negatePoly g)

-- Gives the highest degree of any given polynomial (Use for factoring)

highestDegree :: Polynomial -> Deg
highestDegree (Mono c d)  = d
highestDegree (Add f g) = max (highestDegree f) (highestDegree g)
highestDegree (Mul f g) = highestDegree $ simplify (Mul f g)

countTerms :: Polynomial -> Natural
countTerms (Mono c d) = 1
countTerms (Add f g) = (+) (countTerms f) (countTerms g)
countTerms (Mul f g) = countTerms $ simplify (Mul f g)


factorable :: Polynomial -> Bool 
factorable p
    | highestDegree p > 2 = False 
    | otherwise = True

-- Factoring a monomial : Just return the monomial
-- OTHERWISE 
    -- Check if the max degree is 2 
        -- Yes: Factorise it
        -- No: Error 
quadFactor :: Polynomial -> Polynomial 
quadFactor (Mono c d) = (Mono c d)
quadFactor p = quadFactor' $ simplify p

quadFactor' :: Polynomial -> Polynomial
quadFactor' (Mono c d) = undefined
quadFactor' (Add f g)  = undefined

