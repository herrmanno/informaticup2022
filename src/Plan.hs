module Plan (findBestStateRoute) where

import Prelude hiding (head)
import App (evalApp)
import Context (Context)
import State (State, scoreForState, nextStates, stateIsValid, stateIsFinished, Score)
import Data.Heap (Heap, HeapItem, Prio, Val)
import Data.Heap qualified as H
import Data.Functor.Identity (runIdentity)

data StateScore

instance HeapItem StateScore (State, Score) where
    type Val StateScore (State, Score) = State
    newtype Prio StateScore (State, Score) = P Score deriving (Eq, Ord)

    split (s,sc) = (P sc, s)
    merge (P sc,s) = (s,sc)

findBestStateRoute :: Context c => c -> [State] -> Maybe State
findBestStateRoute c state = go (H.fromList (fmap stateWithScore state)) where
    go :: Heap StateScore (State, Score) -> Maybe State
    go heap = do
        ((s,sc), heap') <- H.view heap
        let ss' = [ stateWithScore s'
                  | ss <- evalApp (nextStates s) c
                  , s' <- ss
                  , runIdentity $ evalApp (stateIsValid s') c
                  ]
        if runIdentity $ evalApp (stateIsFinished s) c
            then Just s
            else go (foldr H.insert heap' ss')
    stateWithScore s = runIdentity $ evalApp ((s,) <$> scoreForState s) c
