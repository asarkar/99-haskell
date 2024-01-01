module Misc2 (fullWords, isIdentifier, sudoku, crossword) where

import qualified Control.Monad as M
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{-
Problem 95: (**) English number words.

On financial documents, like cheques, numbers must sometimes be written in full words.
Example: 175 must be written as one-seven-five.
Write a predicate full-words/1 to print (non-negative) integer numbers in full words.
-}
fullWords :: Int -> String
fullWords 0 = "zero"
fullWords x = (L.intercalate "-" . reverse) $ L.unfoldr go x
  where
    go :: Int -> Maybe (String, Int)
    go 0 = Nothing
    go n = (,left) <$> L.lookup right digit2Word
      where
        (left, right) = n `divMod` 10
    digit2Word =
      [ (1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine")
      ]

{-
Problem 96: (**) Syntax checker.

In a certain programming language (Ada) identifiers are defined by the syntax diagram below.

                              ┌───────────────────────────────┐
                              │                               │
                              │                               │
                             ┌▼┐                              │
                  ┌─────────►└┬┴───────┐                      │
                  │           │        │                      │
                  │           │        ▼                      │
┌──────┬┐     ┌───┴───┐       │       ┌┬┐      ┌───────┐      │    ┌┬──────┐
│begin │┼─────►letter │       │       └┴┴┬─────►letter ├────►┌┼┬───►│end   │
└──────┴┘     └───────┘       │        ▲ │     └───────┘     └┴┘   └┴──────┘
                              │        │ │                    ▲
                           ┌──▼────┐   │ │     ┌───────┐      │
                           │  _    ├───┘ └─────►digit  │      │
                           └───────┘           └───────┴──────┘

Write a function which checks whether a given string is a legal identifier.

ANSWER:
We translate the above syntax diagram into the following state transition diagram.
State are as shown as boxes, and the transitions are shown as edges. The events
causing the transitions are shown as the edge labels.

                                 ┌───────────────────────────────┐
                                 │                               │
                                 │ ┌───────────────┐             │not ""
                                 │ │               │             │
┌──────┐letter  ┌──────┐ not "" ┌▼─┴───┐not '_'┌───▼──┐letter┌───┴──┐
│ begin├────────► S1   ├────────► s2   ├───────►  s3  ├──────►  s4  │
└──────┘        └───┬──┘        └──────┘  '_'  └──────┘digit └───┬──┘
                    │                                            │
                  ""│                                            │""
                    │                                            │
                    │           ┌──────┐                         │
                    └───────────► end  ◄─────────────────────────┘
                                └──────┘

The "not" transitions don't consume any input. For example, from S2,
if the next character is an underscore, then it transitions to S3 with the
remaining string, otherwise it transitions to S3 with the same input that
it had received.
Same for S1-(not "")-> S2, and S4-(not "")-> S2.

Other than the defined events, any other event causes a transition to the
error state (modeled by Nothing in code).
-}

data ParserState = Begin | S1 | S2 | S3 | S4 | End

parseId :: ParserState -> String -> Maybe (ParserState, String)
parseId Begin (x : xs) = if C.isLetter x then Just (S1, xs) else Nothing
parseId S1 "" = Just (End, "")
parseId S1 xs = Just (S2, xs)
parseId S2 (x : xs) = if x == '_' then Just (S3, xs) else Just (S3, x : xs)
parseId S3 (x : xs) = if C.isLetter x || C.isDigit x then Just (S4, xs) else Nothing
parseId S4 "" = Just (End, "")
parseId S4 xs = Just (S2, xs)
parseId _ _ = Nothing

isIdentifier :: String -> Bool
isIdentifier = go Begin
  where
    go s xs = case parseId s xs of
      Just (End, "") -> True
      Just (s', ys) -> go s' ys
      _ -> False

{-
Problem 97: (**) Sudoku.

ANSWER:
Backtracking. The candidates for a cell are the numbers _not_
present in the same row, column, and 3x3 box. Finding the
numbers in the same row and column are easy. For the box,
we first first its top-left coordinate, then iterate
left-to-right and top-to-bottom.

We start from the top-left cell, and for every cell containing
a zero, fill it with one of the candidates, and move on to the
next empty cell. If at any point, there are no candidates
available for a cell, we backtrack, and try the next candidate
for the previous cell.
-}
sudoku :: [[Int]] -> [[Int]]
sudoku grid = (V.toList . V.map VU.toList) (Mb.fromJust soln)
  where
    soln = solve 0 0 initialBoard
    initialBoard = V.fromList $ map VU.fromList grid
    row = flip (V.!)
    col n = V.map (VU.! n)
    box r c board = [get r' c' board | r' <- [x .. x + 2], c' <- [y .. y + 2]]
      where
        x = 3 * (r `div` 3)
        y = 3 * (c `div` 3)
    isValid r c val board =
      val `VU.notElem` row r board
        && val `V.notElem` col c board
        && val `notElem` box r c board
    candidates r c board = [x | x <- [1 .. 9], isValid r c x board]
    get r c board = (board V.! r) VU.! c
    set r c val board =
      V.update board $
        V.singleton
          ( r,
            VU.update (row r board) $
              VU.singleton (c, val)
          )
    solve r c board
      | r > 8 = Just board
      | c > 8 = solve (r + 1) 0 board
      | get r c board /= 0 = solve r (c + 1) board
      | otherwise = M.msum $ do
          x <- candidates r c board
          let b = set r c x board
          return $ solve r (c + 1) b

{-
Problem 98: (***) Nonograms.

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap
is annotated with the respective lengths of its distinct strings of occupied cells.
The person who solves the puzzle must complete the bitmap given only these lengths.

Example:

  Problem:                    Solution:

  |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
  |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
  |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
  |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
  |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
  |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
  |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
  |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
  |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
   1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
   2 1 5 1                     2 1 5 1

3 against the 1st row means there should be 3 consecutive 'X's in the solution.
2 1 against the 2nd row means there should be 2 consecutive 'X's, followed by
_at least_ one space, then another 'X'.

The numbers at the bottom are the constraints for the corresponding columns.

ANSWER: TODO.
-}

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
crossword :: [String] -> [[Char]] -> [[Char]]
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
