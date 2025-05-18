module Misc.P96 (isIdentifier) where

import qualified Data.Char as C

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
States are shown as boxes, and the transitions are shown as arrows between the boxes.
State transitions are caused by reading the next character, if any.
The state transition rules are shown as edge labels.

                                 ┌───────────────────────────────┐
                                 │                               │
                                 │                        not "" │
                                 │                               │
┌──────┐letter  ┌──────┐not ""  ┌▼─────┐not '_'┌──────┐letter┌───┴──┐
│ begin├────────► S1   ├────────► s2   ├───────►  s3  ├──────►  s4  │
└──────┘        └───┬──┘        └──────┘  '_'  └──────┘digit └───┬──┘
                    │                                            │
                  ""│                                          ""│
                    │                                            │
                    │           ┌──────┐                         │
                    └───────────► end  ◄─────────────────────────┘
                                └──────┘

The "not" transitions don't consume any input. For example, from S2,
if the next character is an underscore, then it transitions to S3 with the
remaining string, otherwise it transitions to S3 with the same input that
it had received.
Same for S1-(not "")-> S2, and S4-(not "")-> S2.

"" indicates the end of string.

Reading a character not matching one of the defined transition rules for
that state causes a transition to the error state (not shown in the diagram).
-}

data ParserState = Begin | S1 | S2 | S3 | S4 | End

parseId :: ParserState -> String -> Maybe (ParserState, String)
parseId s xs = case (s, xs) of
  (Begin, x : ys) -> if C.isLetter x then Just (S1, ys) else Nothing
  (S1, "") -> Just (End, "")
  (S1, ys) -> Just (S2, ys)
  (S2, x : ys) -> if x == '_' then Just (S3, ys) else Just (S3, x : ys)
  (S3, x : ys) -> if C.isLetter x || C.isDigit x then Just (S4, ys) else Nothing
  (S4, "") -> Just (End, "")
  (S4, ys) -> Just (S2, ys)
  (_, _) -> Nothing

isIdentifier :: String -> Bool
isIdentifier = go Begin
  where
    go s xs = case parseId s xs of
      Just (End, "") -> True
      Just (s', ys) -> go s' ys
      _ -> False
