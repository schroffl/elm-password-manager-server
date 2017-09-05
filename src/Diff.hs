module Diff where

import Data.List

import Entry

type RemoteState = [RemoteEntry]

data ClientState = ClientState
  { added :: [AddedEntry]
  , modified :: [ModifiedEntry]
  } deriving (Show)

combineState :: RemoteState -> ClientState -> (RemoteState, [ClientInstruction])
combineState rState cState =
  let withNewEntries =
        foldl
          (\(list, instructions) entry ->
             let (newState, instruction) = addEntry list entry
             in (newState, instruction : instructions))
          (rState, [])
          (added cState)
      withModifiedEntries =
        foldl
          (\(list, instructions) entry ->
             let (newState, instruction) = updateEntry list entry
             in (newState, instruction : instructions))
          withNewEntries
          (modified cState)
  in withModifiedEntries

addEntry :: RemoteState -> AddedEntry -> (RemoteState, ClientInstruction)
addEntry rState entry =
  let newEntry = makeRemoteEntry (generateId rState) entry
  in (newEntry : rState, makeInstruction newEntry entry)

updateEntry :: RemoteState -> ModifiedEntry -> (RemoteState, ClientInstruction)
updateEntry rState entry =
  let pair = matchPair rState entry
      winner = pickWinner pair
  in case winner of
       ClientWins _ ->
         let newEntry = makeRemoteEntry 0 entry
             instruction = makeInstruction newEntry entry
         in (replaceSameEntry rState newEntry, instruction)
       RemoteWins rEntry -> (rState, makeInstruction rEntry entry)

replaceSameEntry :: RemoteState -> RemoteEntry -> RemoteState
replaceSameEntry [] _ = []
replaceSameEntry (x:xs) entry
  | rId x == rId entry = entry : xs
  | otherwise = x : replaceSameEntry xs entry

generateId :: RemoteState -> Id
generateId [] = 0
generateId rState = succ $ maximum $ map rId rState