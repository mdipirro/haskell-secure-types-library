{-# LANGUAGE TypeFamilies #-}

module Security.Lattice (LEQ, Proof(Proof)) where

import GHC.Exts (Constraint)

-- Implementation based on Russo et al., 2008

type family LEQ sl sh :: Constraint

data Proof s = Proof
