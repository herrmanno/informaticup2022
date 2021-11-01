module Types where

newtype ID a = ID Int deriving (Show, Eq, Ord)

-- | Num instance of ID type
--   Is only used for making number literas work in test specs.
--   No real Num method is supported on this type!
instance Num (ID a) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = ID . fromInteger
    negate = undefined