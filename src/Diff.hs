module Diff where

import Entry

type ServerState = [Entry]

data ClientState = ClientState
  { added :: [Entry]
  } deriving (Show, Eq)

data Difference = Difference
  {
  } deriving (Show, Eq)

diff :: ServerState -> ClientState -> Difference
diff svState clState = Difference
