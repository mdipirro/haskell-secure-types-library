module Security.SecureFlow (SecureFlow, Hatch, Hatch', open, up, declassifyWith,
makeHatch, declassifyWith') where

import Control.Monad.Identity
import Control.Applicative

-- Implementation based on Russo et al., 2008

import Security.Lattice

-- | SecureFlow: the Identity monad tagged with a proposition allowing to access
-- its value.
newtype SecureFlow s a = SF a
type Hatch s a b = SecureFlow s (a -> b)
type Hatch' s l a b = SecureFlow l (SecureFlow s (a -> b))

instance Functor (SecureFlow s) where
    fmap f (SF x)  = SF $ f x

instance Applicative (SecureFlow s) where
    pure x            = SF x
    (SF f) <*> (SF x) = SF $ f x

instance Monad (SecureFlow s) where
    return       = pure
    (SF a) >>= f = f a

-- | API for handling SecureFlow values. Raising the security level of a value with @up@
-- is possible for everybody. A SecureFlow value can be unwrapped given a proof that
-- accessing the security level of it was SF.

open :: LEQ s s' => Proof s' -> SecureFlow s a -> a
open _ (SF a) = a

up :: LEQ s s' => SecureFlow s a -> SecureFlow s' a
up (SF a) = SF a

-- Returns the second (more restrictive) Hatch version
makeHatch :: (a -> b) -> Hatch' s l a b
makeHatch f = pure $ pure f

-- | Declassification
unsafeCoerceLevels :: LEQ s' s => SecureFlow s a -> SecureFlow s' a
unsafeCoerceLevels (SF x) = SF x

declassifyWith :: (LEQ s k, LEQ s' s) => Hatch k a b -> SecureFlow s a -> SecureFlow s' b
declassifyWith (SF f) s = unsafeCoerceLevels $ do x <- s
                                                  return $ f x

-- Second version
declassifyWith' :: (LEQ s k, LEQ s' s, LEQ l s', LEQ s' l) => Hatch' k l a b -> SecureFlow s a -> SecureFlow s' b
declassifyWith' (SF sf) s = declassifyWith sf s
