module ControlledVisit
    (
      Info(..)
    , getInfo
    , getUsefulContents
    , isDirectory
    ) where

import Control.Monad (filterM, forM, liftM)
import Data.Time.Clock (UTCTime(..))
import System.Directory (Permissions(..), getDirectoryContents,
                         getModificationTime, getPermissions)
import System.FilePath (takeExtension, (</>))
import Control.Exception (SomeException, bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Maybe (isJust)

data Info  = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]

traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

{-isDirectory :: Info -> Bool-}
{-isDirectory = maybe False searchable . infoPerms-}
isDirectory :: Info -> Bool
isDirectory info = maybe False (searchableMaybe . infoPerms) (Just info)
    where searchableMaybe (Just a) = searchable a
          searchableMaybe Nothing = False

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

{-maybeIO :: IO a -> IO (Maybe a)-}
{-maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)-}

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler (Just `liftM` act)
    where handler :: SomeException -> IO (Maybe a)
          handler _ = return Nothing
