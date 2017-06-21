import Criterion.Main

import Src.IO.JSONHandler
import Security.Unsecure

employees = getEmployees
rawEmployees = getRawEmployees

main :: IO ()
main = do rs <- rawEmployees
          vs <- employees
          defaultMain [ bench "raw" $ nf length rs
                      , bench "validated" $ nf length (valid vs)
                      ]
          return ()
       where valid vs = case validate vs of Left es   -> es
                                            Right err -> []
