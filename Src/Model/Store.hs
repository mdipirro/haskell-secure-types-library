{-# LANGUAGE DeriveGeneric #-}

module Src.Model.Store (Store(..), increasePrice, increaseStocks) where

import Data.Aeson
import GHC.Generics

data Store = Store  { productName   :: String
                    , price         :: Double
                    , stocks        :: Int
                    }
                    deriving (Generic)

instance FromJSON Store
instance ToJSON Store
instance Show Store where
  show s =  "\n\n" ++
            "Product Name: " ++ (productName s) ++
            "\nPrice: " ++ (show $ price s) ++
            "\nStocks: " ++ (show $ stocks s) ++
            "\n\n"

increasePrice :: Double -> Store -> Store
increasePrice i s = Store n np st
                    where n   = productName s
                          np  = price s + i
                          st  = stocks s

increaseStocks :: Int -> Store -> Store
increaseStocks i s = Store n p st
                    where n   = productName s
                          p   = price s
                          st  = stocks s + i
