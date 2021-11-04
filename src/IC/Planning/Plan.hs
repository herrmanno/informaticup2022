{-|
Method(s) of calculating a train plan problem's optimal solution
-}
module IC.Planning.Plan (findBestStateRoute) where

import           Data.Functor.Identity (runIdentity)
import           Data.Heap             (Heap, HeapItem, Prio, Val)
import qualified Data.Heap             as H
import           IC.Control.MonadPlan  (evalPlan)
import           IC.Data.Context.Class (Context)
import           IC.Planning.State     (Score, State, nextStates, scoreForState,
                                        stateIsFinished, stateIsValid)

data StateScore

instance HeapItem StateScore (State, Score) where
    type Val StateScore (State, Score) = State
    newtype Prio StateScore (State, Score) = P Score deriving (Eq, Ord)

    split (s,sc) = (P sc, s)
    merge (P sc,s) = (s,sc)

-- |Returns the final state of the optimal solution to a train plan problem
findBestStateRoute
    :: Context c
    => c            -- ^ the initial problem context
    -> [State]      -- ^ the initial states to start from
    -> Maybe State
findBestStateRoute c state = go (H.fromList (fmap stateWithScore state)) where
    go :: Heap StateScore (State, Score) -> Maybe State
    go heap = do
        ((s,sc), heap') <- H.view heap
        let ss' = [ stateWithScore s'
                  | ss <- evalPlan (nextStates s) c
                  , s' <- ss
                  , runIdentity $ evalPlan (stateIsValid s') c
                  ]
        if runIdentity $ evalPlan (stateIsFinished s) c
            then Just s
            else go (foldr H.insert heap' ss')
    stateWithScore s = runIdentity $ evalPlan ((s,) <$> scoreForState s) c
