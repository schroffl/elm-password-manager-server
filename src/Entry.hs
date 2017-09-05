module Entry where

import Data.List

type Id = Int

type Timestamp = Int

data Winner a
  = RemoteWins RemoteEntry
  | ClientWins a
  deriving (Show)

type EntryPair a = (a, Maybe RemoteEntry)

class ComparableEntry a where
  matchPair :: [RemoteEntry] -> a -> EntryPair a
  pickWinner :: (EntryPair a) -> Winner a

class ConvertableEntry a where
  makeRemoteEntry :: Id -> a -> RemoteEntry
  makeInstruction :: RemoteEntry -> a -> ClientInstruction

data RemoteEntry = RemoteEntry
  { rId :: Id
  , rTimestamp :: Timestamp
  } deriving (Show, Read)

-- AddedEntry denotes an Entry that has been added on the client
-- and never been known by the server before
data AddedEntry = AddedEntry
  { aId :: Id
  , aTimestamp :: Timestamp
  } deriving (Show)

instance ComparableEntry AddedEntry where
  matchPair entries entry = (entry, Nothing)
  pickWinner (entry, _) = ClientWins entry

instance ConvertableEntry AddedEntry where
  makeRemoteEntry id entry = RemoteEntry id (aTimestamp entry)
  makeInstruction rEntry aEntry =
    ClientInstruction (aId aEntry) (rId rEntry) (aTimestamp aEntry)

-- ModifiedEntry denotes an Entry that has been changed on the client
data ModifiedEntry = ModifiedEntry
  { mId :: Id
  , mTimestamp :: Timestamp
  } deriving (Show)

instance ComparableEntry ModifiedEntry where
  matchPair entries entry = (entry, find ((== mId entry) . rId) entries)
  pickWinner (clientEntry, maybeRE) =
    case maybeRE of
      Nothing -> ClientWins clientEntry
      Just remoteEntry ->
        if mTimestamp clientEntry > rTimestamp remoteEntry
          then ClientWins clientEntry
          else RemoteWins remoteEntry

instance ConvertableEntry ModifiedEntry where
  makeRemoteEntry _ entry = RemoteEntry (mId entry) (mTimestamp entry)
  makeInstruction rEntry mEntry =
    case pickWinner (mEntry, Just rEntry) of
      ClientWins _ ->
        ClientInstruction (mId mEntry) (mId mEntry) (mTimestamp mEntry)
      RemoteWins _ ->
        ClientInstruction (mId mEntry) (rId rEntry) (rTimestamp rEntry)

-- SyncedEntry denotes an Entry that the client left unmodified 
-- since the last request
data SyncedEntry = SyncedEntry
  { sId :: Id
  , sTimestamp :: Timestamp
  } deriving (Show)

instance ComparableEntry SyncedEntry where
  matchPair entries entry = (entry, find ((== sId entry) . rId) entries)
  pickWinner (clientEntry, maybeRE) =
    case maybeRE of
      Nothing -> ClientWins clientEntry
      Just remoteEntry ->
        if sTimestamp clientEntry > rTimestamp remoteEntry
          then ClientWins clientEntry
          else RemoteWins remoteEntry

data ClientInstruction = ClientInstruction
  { target :: Id
  , uId :: Id
  , uTimestamp :: Timestamp
  } deriving (Show)
