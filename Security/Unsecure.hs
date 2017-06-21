module Security.Unsecure (Unsecure, upure, validate, umap) where

type ValidationFunctions a b = [a -> Maybe b]
newtype Unsecure a b = Unsecure (ValidationFunctions a b, a)

-- | Validate an encapsulated value. If at least one error occures, this
-- function returns a list with every occurred error (Right). Otherwise the
-- actual value is returned (Left).
validate :: Unsecure a b -> Either a [b]
validate (Unsecure (fs, v)) = if null errors
                              then Left v
                              else Right errors
                              where maybes = map (\f -> f v) fs
                                    justs  = filter (\m -> case m of  Nothing -> False
                                                                      Just _  -> True) maybes
                                    errors = map (\(Just m) -> m) justs

-- | Maps a function on the encapsulated value. The function must not change the
-- type since the constraints are defined on the `a` type. That's why, in this
-- way, Unsecure may not be a Functor instance.
umap :: Unsecure a b -> (a -> a) -> Unsecure a b
umap (Unsecure (fs, v)) f = Unsecure (fs, f v)

-- | Since the Unsecure constructor is not exported (for the sake of consistency)
-- `pure` gives the possibility to create an unsecure value. It is actually a
-- kind of factory method.
upure :: a -> ValidationFunctions a b -> Unsecure a b
upure value fs = Unsecure (fs, value)
