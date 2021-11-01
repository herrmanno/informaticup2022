module Plan where

import Prelude hiding (head)
import Context (Context)
import State (State, scoreForState, nextStates, stateIsValid, stateIsFinished)
import Data.Heap (Heap, HeapItem, Prio, Val)
import qualified Data.Heap as H
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NL
import Control.Monad.Trans.State (execStateT, evalStateT)
import Data.Functor.Identity (runIdentity)

data StateScore

instance HeapItem StateScore State where
    newtype Prio StateScore State = P { prio :: Double } deriving (Eq, Ord)
    type Val StateScore State = State

    split s = (P $ scoreForState s, s)
    merge (_,s) = s

findBestStateRoute :: Context c => c -> [State] -> Maybe State
findBestStateRoute c s = go (H.fromList s) where
    go :: Heap StateScore State -> Maybe State
    go heap =
        let Just (s, heap') = H.view heap
            ss' = [ s'
                  | ss <- evalStateT (nextStates s) c
                  , s' <- ss
                  , valid <- evalStateT (stateIsValid s') c
                  , valid
                  ]
        in if runIdentity $ evalStateT (stateIsFinished s) c
            then Just s
            else go (foldr H.insert heap' ss')
