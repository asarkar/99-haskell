module Misc.P99 where

import qualified Control.Monad as M
import qualified Data.Char as C
import qualified Data.Maybe as Mb
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{-
Problem 99: (***) Crossword puzzle.AnnotatedTree.

Given an empty (or almost empty) framework of a crossword puzzle and a set of words.
The problem is to place the words into the framework.

Words are strings (character lists) of at least two characters.
A horizontal or vertical sequence of character places in the crossword puzzle framework
is called a site. Our problem is to find a compatible way of placing words onto sites.

For efficiency reasons it is important, at least for larger puzzles, to sort the words
and the sites in a particular order. For this part of the problem, the solution of P28
may be very helpful.

ANSWER:
Backtracking.

- Instead of repeated updates of the grid, using modifiable array/vector would probably
be faster.
- Another improvement (?) could be to find the sites and match them with words of similar
  lengths.
-}
crossword :: [String] -> [String] -> [String]
crossword words' grid = (V.toList . V.map VU.toList) (Mb.fromJust soln)
  where
    soln = solve 0 0 words' initialBoard
    initialBoard = V.fromList $ map VU.fromList grid
    m = V.length initialBoard
    n = VU.length $ V.head initialBoard
    row = flip (V.!)
    col c = V.map (VU.! c)
    get r c board = (board V.! r) VU.! c
    -- The word can fit starting at cell (r, c) if
    -- there's enough empty space for the whole word,
    -- or there's a prefix match, and there's space
    -- for the remaining suffix.
    isFitHorz word r c board =
      c <= (n - k)
        && all (\(x, y) -> C.isSpace x || x == y) ys
      where
        k = length word
        xs = VU.slice c k (row r board)
        ys = zip (VU.toList xs) word
    isFitVert word r c board =
      r <= (m - k)
        && all (\(x, y) -> C.isSpace x || x == y) ys
      where
        k = length word
        xs = col c $ V.slice r k board
        ys = zip (V.toList xs) word
    placeHorz word r c board =
      V.update board $
        V.singleton
          ( r,
            VU.update (row r board) $
              VU.fromList (zip [c ..] word)
          )
    placeVert word r c board =
      V.update board $
        V.fromList $
          zipWith (\i x -> (i, updateRow i x)) [r ..] word
      where
        updateRow i x =
          VU.update (row i board) $
            VU.singleton (c, x)

    solve _ _ [] board = Just board
    solve r c w@(x : xs) board
      | r >= m = Nothing
      | c >= n = solve (r + 1) 0 w board
      | (== '.') (get r c board) = solve r (c + 1) w board
      | horzFit || vertFit = do
          let placements = [(horzFit, placeHorz), (vertFit, placeVert)]
          let res = M.msum $ do
                (fit, place) <- placements
                M.guard fit
                -- Place the word, and start from
                -- the top with the remaining words.
                let b = place x r c board
                return $ solve 0 0 xs b
          case res of
            -- Placing the word here didn't work,
            -- find another place.
            Nothing -> solve r (c + 1) w board
            b -> b
      | otherwise = solve r (c + 1) w board
      where
        horzFit = isFitHorz x r c board
        vertFit = isFitVert x r c board
