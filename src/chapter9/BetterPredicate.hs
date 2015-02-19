import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import Control.Exception (IOException, bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile, Handle)

-- the function we wrote yesterday
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath        -- path to directory entry
               -> Permissions    -- permissions
               -> Maybe Integer  -- file size (Nothing if not file)
               -> UTCTime        -- last modified
               -> Bool


betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing)::IOError -> IO (Maybe Integer) ) $
    bracket (openFile path ReadMode) hClose (f)
        where
            f :: Handle -> IO (Maybe Integer)
            f h = do
                size <- hFileSize h
                return (Just size)
