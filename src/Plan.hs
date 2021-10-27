module Plan where

import Prelude hiding (head)
import State (State, scoreForState, nextStates, stateIsValid, stateIsFinished)
import Data.Heap (Heap, HeapItem, Prio, Val)
import qualified Data.Heap as H
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NL

data StateScore

instance HeapItem StateScore (NonEmpty State) where
    newtype Prio StateScore (NonEmpty State) = P { prio :: Double } deriving (Eq, Ord)
    type Val StateScore (NonEmpty State) = (NonEmpty State)

    split s = (P $ scoreForState (NL.head s), s)
    merge (_,s) = s

-- findBestStateRoute :: State -> [State]
findBestStateRoute s = go (H.singleton $ NL.fromList [s]) where
    go :: Heap StateScore (NonEmpty State) -> Maybe (NonEmpty State)
    go heap =
        let Just (s, heap') = H.view heap
            ss' = [ s' <| s | s' <- nextStates (NL.head s), stateIsValid s']
        in if stateIsFinished (NL.head s)
            then Just s
            else go (foldr H.insert heap' ss')
