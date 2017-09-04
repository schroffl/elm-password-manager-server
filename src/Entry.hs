module Entry where

data Entry = Entry
  { id :: Int
  , meta :: EntryMeta
  } deriving (Show, Eq)

data EntryMeta = EntryMeta
  { isNew :: Bool
  , isSynced :: Bool
  , lastModified :: Int
  } deriving (Show, Eq)

entryIsNew :: Entry -> Bool
entryIsNew = isNew . meta

newEntry :: Entry
newEntry = Entry 0 $ EntryMeta True False 104

syncedEntry :: Entry
syncedEntry = Entry 1 $ EntryMeta False True 100
 
modifiedEntry :: Entry
modifiedEntry = Entry 1 $ EntryMeta False False 374