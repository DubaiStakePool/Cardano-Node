{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Concurrent.Extra (Lock, withLock)
import           Control.Monad (unless)
import           Control.Monad.Extra (ifM)
import qualified Data.ByteString as BS
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import           System.Directory.Extra (listFiles)
import           System.FilePath ((</>))

import           Cardano.Logging (TraceObject (..))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils

-- | Append the list of 'TraceObject's to the latest log via symbolic link.
--
-- It is technically possible that, during writing in the current log,
-- the rotator's thread will check if the current log is full and, if so,
-- the symbolic link will be switched to the new log file and writing can
-- be interrupted. To prevent it, we use 'Lock'.
writeTraceObjectsToFile
  :: NodeName
  -> Lock
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile nodeName currentLogLock rootDir format traceObjects = do
  rootDirAbs <- makeAbsolute rootDir
  let converter = case format of
                    ForHuman   -> traceObjectToText
                    ForMachine -> traceObjectToJSON
  let itemsToWrite = mapMaybe converter traceObjects
  unless (null itemsToWrite) $ do
    pathToCurrentLog <- getPathToCurrentlog nodeName rootDirAbs format
    let preparedLine = TE.encodeUtf8 $ T.concat itemsToWrite
    withLock currentLogLock $
      BS.appendFile pathToCurrentLog preparedLine

-- | Returns the path to the current log. Prepares the structure for the log files if needed:
--
--   /rootDir
--     /subDirForNode1
--       logs from node 1
--     /subDirForNode2
--       logs from node 2
--     ...
--     /subDirForNodeN
--       logs from node N
--
getPathToCurrentlog
  :: NodeName
  -> FilePath
  -> LogFormat
  -> IO FilePath
getPathToCurrentlog nodeName rootDirAbs format =
  ifM (doesDirectoryExist subDirForLogs)
    getPathToCurrentLogIfExists
    prepareLogsStructure
 where
  subDirForLogs = rootDirAbs </> T.unpack nodeName

  getPathToCurrentLogIfExists = do
    logsWeNeed <- filter (isItLog format) <$> listFiles subDirForLogs
    if null logsWeNeed
      then createEmptyLog subDirForLogs format
      -- We can sort the logs by timestamp, the biggest one is the latest one.
      else return $ subDirForLogs </> maximum logsWeNeed

  prepareLogsStructure = do
    -- The root directory (as a parent for subDirForLogs) will be created as well if needed.
    createDirectoryIfMissing True subDirForLogs
    createEmptyLog subDirForLogs format

traceObjectToText :: TraceObject -> Maybe T.Text
traceObjectToText TraceObject{toHuman} =
  case toHuman of
    Nothing -> Nothing
    Just msgForHuman -> Just $ msgForHuman <> nl

traceObjectToJSON :: TraceObject -> Maybe T.Text
traceObjectToJSON TraceObject{toMachine} =
  Just $ toMachine <> nl

