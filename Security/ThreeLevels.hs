{-# LANGUAGE TypeFamilies #-}

module Security.ThreeLevels (Low, Medium, High, low, medium) where

import Security.Lattice

data Low    = L
data Medium = M
data High   = H

type instance (LEQ Low Low) = ()
type instance (LEQ Low Medium) = ()
type instance (LEQ Low High) = ()
type instance (LEQ Medium Medium) = ()
type instance (LEQ Medium High) = ()
type instance (LEQ High High) = ()

low :: Proof Low
low = Proof

medium :: Proof Medium
medium = Proof

-- Not exported
high :: Proof High
high = Proof
