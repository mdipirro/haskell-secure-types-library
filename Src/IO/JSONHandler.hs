module Src.IO.JSONHandler (getEmployees, saveEmployees, getCredentials,
getSecureEmployees, getStores, getRawEmployees) where

-- | In this module there are some useful function for reading values from JSONHandler
-- files. Those files are used as storage.

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Security.Unsecure
import Security.SecureFlow
import Security.ThreeLevels
import Security.SecureComputation
import Src.Model.Employee
import Src.Model.SecureEmployee
import Src.Model.Credential
import Src.Model.Store
import qualified Src.Validation.Employee as EmployeeV

employeesFile :: FilePath
employeesFile = "data/employees.json"

storesFile :: FilePath
storesFile = "data/stores.json"

credentialsFile :: FilePath
credentialsFile = "data/credentials.json"

saveEmployees es = BSL.writeFile employeesFile (encodePretty es)

getEmployees :: IO (Unsecure [Employee] EmployeeV.EmployeeError)
getEmployees =  do  json <- eitherDecode <$> BSL.readFile employeesFile
                    case json of  Left err -> return (upure [] [])
                                  Right es -> return (upure es (EmployeeV.validate))

getRawEmployees :: IO [Employee]
getRawEmployees = do  json <- eitherDecode <$> BSL.readFile employeesFile
                      case json of  Left err -> return []
                                    Right es -> return es

getCredentials :: IO (SecureFlow High [Credential])
getCredentials = do json <- eitherDecode <$> BSL.readFile credentialsFile
                    case json of  Left err -> return (fail "")
                                  Right cs -> return (pure cs)

getSecureEmployees :: IO [SEmployee]
getSecureEmployees = do json <- eitherDecode <$> BSL.readFile employeesFile
                        case json of  Left err -> return []
                                      Right es -> return $ map fromEmployee es

getStores :: IO (SecureComputation P [Store])
getStores = do  json <- eitherDecode <$> BSL.readFile storesFile
                case json of  Left err -> return $ spure []
                              Right ss -> return $ spure ss
