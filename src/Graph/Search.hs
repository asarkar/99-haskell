{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graph.Search where

import qualified Control.Monad as M
import qualified Control.Monad.State as S
import Data.Set (Set)
import qualified Data.Set as Set
import Graph.Graph

data Search a = Search
  { start :: a,
    -- shouldInclude visited vertex
    expand :: Set a -> a -> [a],
    isDone :: Set a -> a -> Bool
  }

-- Backtracking DFS that may visit the same
-- vertex more than once from another path.
search :: (Ord a) => Search a -> [[a]]
search (Search {start, expand, isDone}) = go Set.empty start
  where
    go visited u
      | isDone visited u = [[u]]
      | otherwise = do
          let visited' = Set.insert u visited
          v <- expand visited' u
          xs <- go visited' v
          M.guard (xs /= [])
          return $ u : xs

{-
Creates a spanning forest of the part of the graph reachable from the
listed vertices, obtained from a depth-first search of the graph starting
at each of the listed vertices in order.

FlexibleContexts needs to be enabled because the signature of go fixes
the 's' of (MonadState s m) to a Set, which is not allowed in Haskell 98.

  go :: S.MonadState (Set a) m => [a] -> m [[a]]
-}
dfs :: (Ord a) => Graph a -> [a] -> [[a]]
dfs g vs0 = S.evalState (go vs0) Set.empty
  where
    go [] = return []
    go (v : vs) = do
      visited <- S.get
      if v `Set.member` visited
        then go vs
        else do
          S.modify (Set.insert v)
          let adjacent = neighbors g v
          xs <- M.join <$> go adjacent
          ys <- go vs
          return $ (v : xs) : ys
