module Entry where

data Entry = Entry
  { id :: Int
  , meta :: EntryMeta
  } deriving (Show, Eq)

data EntryMeta = EntryMeta
  { isLocal :: Bool
  , isModified :: Bool
  , modifiedAt :: Int
  } deriving (Show, Eq)