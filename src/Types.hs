module Types (ID(..)) where

-- TODO: derive Num
newtype ID a = ID Int deriving (Eq, Ord)

instance Show (ID a) where
    show (ID i) = show i

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