module Src.Validation.Employee (validate, EmployeeError(Birthdate, Name, Email)) where

import Text.Regex.Posix
import Text.Email.Validate (isValid)
import Data.ByteString.Char8 (pack)
import Data.List (partition)
import Src.Model.Employee

-- !Here there are some funcion to validate employees values.

data EmployeeError =  Birthdate String | Name String | Email String | Salary String
                      deriving Show

nameRegex   = "^[a-zA-Z =]*$"
dateRegex   = "(^(((0[1-9]|1[0-9]|2[0-8])[/](0[1-9]|1[012]))|((29|30|31)[/](0[13578]|1[02]))|((29|30)[/](0[4,6,9]|11)))[/](19|[2-9][0-9])[0-9][0-9]$)|(^29[/]02[/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)"

validationFold :: [Employee] -> (Employee -> Bool) -> Maybe Employee
validationFold [] _     = Nothing
validationFold (e:es) p = if p e
                          then validationFold es p
                          else Just e

validateDate :: [Employee] -> Maybe EmployeeError
validateDate es = case validationFold es (\e -> birthdate e =~ dateRegex) of
                    Nothing -> Nothing
                    Just e  -> Just (Birthdate ("Incorrect date " ++ birthdate e))

validateName :: [Employee] -> Maybe EmployeeError
validateName es = case validationFold es (\e -> firstName e =~ nameRegex && lastName e =~ nameRegex) of
                    Nothing -> Nothing
                    Just e  -> Just (Name ("Incorrect name " ++ (firstName e) ++ " " ++ (lastName e)))

validateEmail :: [Employee] -> Maybe EmployeeError
validateEmail es =  case validationFold es (\e -> isValid . pack . email $ e) of
                      Nothing -> Nothing
                      Just e  -> Just (Email ("Invalid email " ++ (email e)))

salaryLowerThanLeaders :: [Employee] -> Maybe EmployeeError
salaryLowerThanLeaders es = case validationFold nls (\nl -> length (filter (\l -> salary l < salary nl) ls) == 0) of
                              Nothing -> Nothing
                              Just e  -> Just (Salary (firstName e ++ " " ++ (lastName e) ++ "'s salary is too high"))
                            where (ls, nls) = partition (\e -> leader e) es


validate :: [[Employee] -> Maybe EmployeeError]
validate = [validateDate, validateName, validateEmail, salaryLowerThanLeaders]
