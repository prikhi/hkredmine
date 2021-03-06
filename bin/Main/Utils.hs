{-|
-
- This module contains utility functions used by the `hkredmine` executable
-
-}
module Main.Utils
        ( getWidth
        , diffTimeToHours
        , readFileOrExit
        , writeTimeFile
        , getAccount
        , appFileExists
        , writeAppFile
        , readAppFile
        , removeAppFile
        , getAppDir
        ) where

import Prelude hiding (readFile)

import Control.Exception            (catch, throwIO)
import Control.Monad                (liftM)
import Data.Time.Clock              (DiffTime)
import Data.Time.Clock.POSIX        (getPOSIXTime)
import System.Console.Terminal.Size (size, width)
import System.Directory             (getAppUserDataDirectory, removeFile,
                                     doesFileExist)
import System.Exit                  (exitFailure)
import System.IO                    (openFile, IOMode(..), hClose, hPutStr,
                                     hFlush)
import System.IO.Strict             (readFile)
import System.IO.Error              (isDoesNotExistError)


-- | Retrieve the output width. Defaults to the terminal width, falling
-- back to 80 characters if the width is unavailable.
getWidth :: IO Integer
getWidth    = liftM (maybe 80 width) size

-- | Turn a 'DiffTime' into a Double representing the nubmer of hours.
diffTimeToHours :: DiffTime -> Double
diffTimeToHours dt      = fromIntegral (round dt :: Integer) / 3600.0

-- | Retrieve the currently used account if one exists.
getAccount :: IO (Maybe String)
getAccount              = do
        tracking        <- appFileExists "account"
        if tracking
            then Just <$> readAppFile "account"
            else return Nothing


-- General File/Directory Operations
-- | Read from the specified file, returning the contents or exiting with
-- an error.
readFileOrExit :: String    -- ^ The File Name
               -> String    -- ^ The Error Message to print before exiting.
               -> IO String
readFileOrExit fn err   =
        catch (readAppFile fn)
              ((\_  -> putStrLn err >> exitFailure) :: IOError -> IO String)

-- | Write the curent time to a file in the UserData directory.
writeTimeFile :: String     -- ^ The File Name
              -> IO ()
writeTimeFile fn        = do
        currentTime <- getPOSIXTime
        writeAppFile fn $ show (round currentTime :: Integer)

-- | Check whether a File in the App's UserData directory exists.
appFileExists :: String -> IO Bool
appFileExists fn        = do
        appDir      <- getAppDir
        doesFileExist $ appDir ++ "/" ++ fn


-- | Write a string to a file in the Apps's UserData directory.
writeAppFile :: String      -- ^ The File Name
             -> String      -- ^ The File's Content
             -> IO ()
writeAppFile fn content = do
        appDir      <- getAppDir
        fileHandle  <- openFile (appDir ++ fn) WriteMode
        hPutStr fileHandle content
        hFlush fileHandle
        hClose fileHandle

-- | Read a file in the App's UserData directory.
readAppFile :: String       -- ^ The File Name
            -> IO String    -- ^ The File's Content
readAppFile  fn         = do
        appDir      <- getAppDir
        readFile (appDir ++ fn)

-- | Remove a file from the hkredmine UserData directory.
removeAppFile :: FilePath -> IO ()
removeAppFile fn        = do
        appDir <- getAppDir
        writeAppFile fn ""        -- Overcoming laziness problems
        removeFile (appDir ++ fn) `catch` handleExists
        where handleExists e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

-- | Retrieve the App's UserData directory
getAppDir :: IO FilePath
getAppDir   = (++ "/") <$> getAppUserDataDirectory "hkredmine"
