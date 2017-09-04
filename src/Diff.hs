module Diff where

import Data.List

import Entry

-- LocalState refers to the database that is maintained by the client
-- whereas RemoteState refers to the database on the server
type RemoteState = [Entry]

data LocalState = LocalState
  { added :: [Entry]
  , modified :: [Entry]
  } deriving (Show, Eq)

data Winner
  = RemoteWins Entry
  | LocalWins Entry
  deriving (Show, Eq)

diff :: RemoteState -> LocalState -> [Winner]
diff rState lState =
  let addedEntries = map LocalWins (added lState)
      modifiedEntries =
        map pickWinner $ map (findMatch rState) (modified lState)
  in modifiedEntries ++ addedEntries

applyDiff :: RemoteState -> [Winner] -> RemoteState
applyDiff = foldl applyWinner

applyWinner :: RemoteState -> Winner -> RemoteState
applyWinner rState winner =
  case winner of
    LocalWins entry ->
      let syncedEntry =
            entry {meta = (meta entry) {isNew = False, isSynced = True}}
      in if entryIsNew entry
           then syncedEntry {Entry.id = generateId rState} : rState
           else updateEntry rState syncedEntry
    RemoteWins _ -> rState

updateEntry :: RemoteState -> Entry -> RemoteState
updateEntry rState entry =
  map
    (\e ->
       if isSame e entry
         then entry
         else e)
    rState

findMatch :: RemoteState -> Entry -> (Entry, Maybe Entry)
findMatch rState entry = (entry, find (isSame entry) rState)

isSame :: Entry -> Entry -> Bool
isSame e1 e2 =
  if (isNew $ meta e1) || (isNew $ meta e2)
    then False
    else Entry.id e1 == Entry.id e2

pickWinner :: (Entry, Maybe Entry) -> Winner
pickWinner (localEntry, maybeRE) =
  case maybeRE of
    Nothing -> LocalWins localEntry
    Just remoteEntry ->
      let remoteTime = lastModified $ meta remoteEntry
          localTime = lastModified $ meta localEntry
      in if localTime > remoteTime
           then LocalWins localEntry
           else RemoteWins remoteEntry

generateId :: RemoteState -> Int
generateId = succ . maximum . map Entry.id

testRS :: RemoteState
testRS = [Entry 1 (EntryMeta False True 200), Entry 0 (EntryMeta False True 100)]

testLS :: LocalState
testLS = LocalState [newEntry] [Entry 0 (EntryMeta False False 200)]
