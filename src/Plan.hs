module Plan where

import Prelude hiding (head)
import State (State, scoreForState, nextStates, stateIsValid, stateIsFinished)
import Data.Heap (Heap, HeapItem, Prio, Val)
import qualified Data.Heap as H
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NL

data StateScore

instance HeapItem StateScore State where
    newtype Prio StateScore State = P { prio :: Double } deriving (Eq, Ord)
    type Val StateScore State = State

    split s = (P $ scoreForState s, s)
    merge (_,s) = s

findBestStateRoute :: [State] -> Maybe State
findBestStateRoute s = go (H.fromList s) where
    go :: Heap StateScore State -> Maybe State
    go heap =
        let Just (s, heap') = H.view heap
            ss' = [s' | s' <- nextStates s, stateIsValid s']
        in if stateIsFinished s
            then Just s
            else go (foldr H.insert heap' ss')
