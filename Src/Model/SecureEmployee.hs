module Src.Model.SecureEmployee(SEmployee, viewPublicDetails, fromEmployee,
firstName, lastName, birthdate, salary, email, leader, increaseSalary) where

import qualified Src.Model.Employee as E
import Security.SecureFlow
import Security.Lattice
import Security.ThreeLevels

-- | A secure version of Employee where every field is given a specific security
-- level.

data SEmployee = SEmployee  { firstName  :: SecureFlow Low String
                            , lastName   :: SecureFlow Low String
                            , birthdate  :: SecureFlow Low String
                            , salary     :: SecureFlow High Int
                            , email      :: SecureFlow Medium String
                            , leader     :: SecureFlow Low Bool
                            }

-- Cast function from a ''normal'' Employee
fromEmployee :: E.Employee -> SEmployee
fromEmployee e = SEmployee  (pure $ E.firstName e)
                            (pure $ E.lastName e)
                            (pure $ E.birthdate e)
                            (pure $ E.salary e)
                            (pure $ E.email e)
                            (pure $ E.leader e)

-- | In order to view public details (i.e. all but salary) a `medium` ticked is
-- required. Since `email` has a `Medium` level, a `low` proof is not enough.
viewPublicDetails :: LEQ Medium s => Proof s -> SEmployee -> String
viewPublicDetails _ se = "Firstname: " ++ (open' $ firstName se) ++
                              "\nLastname: " ++ (open' $ lastName se) ++
                              "\nBirthdate: " ++ (open' $ birthdate se) ++
                              "\nEmail address: " ++ (open' $ email se) ++
                              "\nIs a leader?: " ++ show (open' $ leader se)
                              where open' e = open medium e

-- | A simple function for increasing the salary without reading it. Here the
-- functorial `fmap` function is used since the salary is encapsulated into a
-- SecureFlox box.
increaseSalary :: Int -> SEmployee -> SEmployee
increaseSalary i se = SEmployee f l b s e le
                      where f = firstName se
                            l = lastName se
                            b = birthdate se
                            s = fmap (\s -> s+i) $ salary se
                            e = email se
                            le = leader se
