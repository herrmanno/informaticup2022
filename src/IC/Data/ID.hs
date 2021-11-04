{-|
Numerical ID type
-}
module IC.Data.ID (ID(..)) where

-- |A Int-Wrapper with a phantom type for distinguishing ID's of different domain types
newtype ID a = ID Int deriving (Eq, Ord, Num)

instance Show (ID a) where
    show (ID i) = show i
