{-# LANGUAGE ScopedTypeVariables #-}
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
{-import System.IO(catch)-}
import qualified Control.Exception as Err
{-import Control.Exception(finally, catch)-}

-- main entry point. We work with a temp file in myAction
main :: IO ()
main = withTempfile "myTemp.txt" myAction

{- the guts of the program. Called with the path
 - and hanlde of a temp file.
 - When the function exits, that file will be 
 - closed and deleted because myAction was called
 - from withTempfile. -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- display a greeting
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname

        -- initial position
        pos <- hTell temph
        putStrLn $ "The initial position is " ++ show pos

        -- write data to the temp file
        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++
                   show (length tempdata) ++ " bytes: " ++
                   tempdata
        hPutStrLn temph tempdata

        -- get the new position. Does not modify pos
        -- but actually has the name "pos" point to a different
        -- value for the remainder of the do block
        pos <- hTell temph
        putStrLn $ "After writing, my new position is " ++ show pos

        -- seek to the beginning of the file & display
        putStrLn $ "The file content is: " 
        hSeek temph AbsoluteSeek 0

        -- hGetContents performs a lazy read of the entire file
        c <- hGetContents temph

        -- copy the file byte-for-byte to stdout, followed by \n
        putStrLn c

        putStrLn $ "Which could be expressed as this Haskell literal:"
        print c

{- Function takes two parameters: a filename pattern and another
     function. It will create a tempfile and pass the name and handle
     of that file to the given function

     The temp file is created with openTempFile. The directory is the one indicated by
     getTemporaryDirectory, or, if the system has no notion of a temporary directory,
     "." is used. The given pattern is given to openTempFile.

     After the fiven function terminates, even if it terminates due to an exception,
     the Handle is closed and the file is deleted 
-}
withTempfile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempfile pattern func = 
    do -- the libraru ref says that getTemporaryDirectory may raise an
        -- exception on systems that have no notion of a temporary directory.
        -- So, we run getTemporaryDirectory under catch. catch takes
        -- two functions: one to run, and a different one to run if the first raised
        -- an exception. If getTemporaryDirectory raised an exception, just use
        -- "." (the cwd)
        tempdir <- Err.catch (getTemporaryDirectory) (\(_::Err.IOException) -> return ".")
        (tempfile, temph) <- openTempFile tempdir pattern

        -- Call (func tempfile temph) to perform the action to the temporary
        -- file. finally takes two actions: the first is the action to run.
        -- The second is an action to run after the first, regardless of
        -- whether the first action raised an exception. This way, we ensure
        -- the temporary file is always deleted. The return value from finally
        -- is the first action's return value.
        Err.finally (func tempfile temph)
                (do hClose temph
                    removeFile tempfile)
