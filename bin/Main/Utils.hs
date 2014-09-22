{-|
-
- This module contains utility functions used by the `hkredmine` executable
-
-}
module Main.Utils
        ( getAppDir
        , writeAppFile
        , readAppFile
        , removeAppFile
        , readFileOrExit
        , writeTimeFile
        ) where

import Prelude hiding (readFile)

import Control.Applicative      ((<$>))
import Control.Exception        (catch, throwIO)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import System.Directory         (getAppUserDataDirectory, removeFile)
import System.Exit              (exitFailure)
import System.IO                (openFile, IOMode(..), hClose, hPutStr,
                                 hFlush)
import System.IO.Strict         (readFile)
import System.IO.Error          (isDoesNotExistError)


-- | Retrieve the App's UserData directory
getAppDir :: IO FilePath
getAppDir = (++ "/") <$> getAppUserDataDirectory "hkredmine"

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
removeAppFile fn = do
        appDir <- getAppDir
        writeAppFile fn ""        -- Overcoming laziness problems
        removeFile (appDir ++ fn) `catch` handleExists
        where handleExists e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e


-- | Read from the specified file, returning the contents or exiting with
-- an error.
readFileOrExit :: String    -- ^ The File Name
               -> String    -- ^ The Error Message to print before exiting.
               -> IO String
readFileOrExit fn err   =
        catch (readAppFile fn)
              ((\_  -> putStrLn err >> exitFailure) :: IOError -> IO String)

-- | Write the curent time to a file.
writeTimeFile :: String     -- ^ The File Name
              -> IO ()
writeTimeFile fn        = do
        currentTime <- getPOSIXTime
        writeAppFile fn $ show (round currentTime :: Integer)
