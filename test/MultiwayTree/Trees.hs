module MultiwayTree.Trees where

import MultiwayTree.MultiwayTree (Tree (..))

t1 :: Tree Char
t1 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

tree5 :: Tree Char
tree5 =
  Node
    'a'
    [ Node
        'f'
        [ Node 'g' []
        ],
      Node 'c' [],
      Node
        'b'
        [ Node 'd' [],
          Node 'e' []
        ]
    ]

{-
tree5:
           ┌──┐
 ┌─────────┤a ├────────┐
 │         └─┬┘        │
 │           │         │
 │           │         │
┌┴─┐       ┌─┴┐       ┌┴─┐
│f │       │c │  ┌────┤b ├─────┐
└┬─┘       └──┘  │    └──┘     │
 │               │             │
 │               │             │
┌┴─┐           ┌─┴┐           ┌┴─┐
│g │           │d │           │e │
└──┘           └──┘           └──┘

-}
