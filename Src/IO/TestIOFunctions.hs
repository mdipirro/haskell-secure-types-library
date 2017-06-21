module Src.IO.TestIOFunctions where

import Data.Char
import Security.Unsecure
import Security.SecureComputation

data TestIOError =  NegativeNumber | NonNumeric
                    deriving Show

-- | Constraints the String with some validation functions. A natural number must
-- be a number and greater, or equal, to zero.
getNat :: IO (Unsecure String TestIOError)
getNat = do n <- getLine
            return $ upure n   [
                                  (\s ->  if null $ dropWhile isDigit s
                                          then Nothing
                                          else Just NonNumeric),
                                  (\n ->  if read n >= 0
                                          then Nothing
                                          else Just NegativeNumber)
                                ]

-- | In this case the String is not constrainted, but it is marked as tainted.
getUnpureNat :: IO (SecureComputation T String)
getUnpureNat = do n <- getLine
                  return $ spure n
