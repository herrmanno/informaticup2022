module IC.Data.ID (ID(..)) where

newtype ID a = ID Int deriving (Eq, Ord, Num)

instance Show (ID a) where
    show (ID i) = show i
