{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : HKRedmine
Description     : A Redmine CLI Client
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX
-}
module Main (main) where

import Control.Monad            (void, when)
import Control.Monad.IO.Class   (liftIO)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import System.Environment       (getArgs)
import System.Directory         (doesFileExist, getAppUserDataDirectory,
                                 createDirectoryIfMissing)

import Web.HTTP.Redmine

-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main              = do
        _       <- initializeApp
        args    <- getArgs
        defCfg  <- defaultRedmineConfig
        let cfg = defCfg { redURL = ""
                         , redAPI = "" }
        result  <- runRedmine cfg $ commandHandler args
        case result of
            Right _  -> putStr   "OK:    " >> print result
            Left err -> putStrLn "ERROR: " >> print err

commandHandler :: [String] -> Redmine ()
commandHandler args = case args of
        ["projects"]                    -> printProjects
        ["print", "project", projectID] -> printProject $ read projectID
        ["print", "issues"]             -> printIssues
        ["details", _]                  -> error "Not Yet Implemented"
        ["track", "project", projectID] -> liftIO . trackProject $ read projectID
        ["track", "issue", issueID]     -> liftIO . trackIssue $ read issueID
        ["startwork", issueID]          -> liftIO . startTimeTracking $ read issueID
        _                               -> liftIO printUsage

-- | Create the Data Directory & Files for the Application
initializeApp :: IO ()
initializeApp = do
        appDir  <- getAppUserDataDirectory "hkredmine"
        _       <- createDirectoryIfMissing True appDir
        let projectFile = appDir ++ "/project"
        projectFileExists <- doesFileExist projectFile
        when (not projectFileExists) (writeFile projectFile "")

-- | Print the Program's Usage Text
printUsage :: IO ()
printUsage        = do
        let message = [ "HKRedmine - Redmine CLI Client"
                      , ""
                      , "Usage:"
                      , "hkredmine command <args>"
                      , ""
                      , "Commands:"
                      , "projects                 -- Print All Projects"
                      , "print project <id>       -- Print the Details of a Specific Project"
                      , "print issues             -- Print All Issues of the Tracked Project"
                      , "print myissues           -- Print Your Issues of the Tracked Project"
                      , "track project <id>       -- Track the Specified Project"
                      , "track issue <id>         -- Track the Specified Issue"
                      , "startwork <id>           -- Start Tracking Time"
                      , ""
                      ]
        mapM_ putStrLn message


-- Displaying Data

-- | Print All Projects
printProjects :: Redmine ()
printProjects           = do Projects ps <- getProjects
                             liftIO . putStrLn . projectsTable $ ps

-- | Print A Single Project
printProject :: Integer -> Redmine ()
printProject projectID  = do
        Projects ps <- getProjects
        void . liftIO . sequence $ map (\p -> when (projectId p == projectID)
                                              $ putStrLn . projectDetail $ p) ps

---- | Print All Issues of a Project
printIssues :: Redmine ()
printIssues             = do
        projectID <- liftIO getTrackedProject
        Issues is <- getAllIssues projectID
        liftIO . putStrLn . issuesTable $ is


-- Project/Issue/Time Tracking

-- | Track a Project By Writing It's ID to a File
trackProject :: Integer -> IO ()
trackProject projectID  = do
        appDir    <- getAppDir
        writeFile (appDir ++ "/project") $ show projectID ++ "\n"

-- | Retrieve the Currently Tracked Project
getTrackedProject :: IO Integer
getTrackedProject       = do
        appDir    <- getAppDir
        projectID <- readFile (appDir ++ "/project")
        return $ read projectID

-- | Retrieve the Currently Tracked Issue
getTrackedIssue :: IO Integer
getTrackedIssue         = do
        appDir    <- getAppDir
        issueID   <- readFile (appDir ++ "/issue")
        return $ read issueID

-- | Track Time by Writing the Current POSIX Time to a File
startTimeTracking :: Integer -> IO ()
startTimeTracking i     = do
        _         <- trackIssue i
        appDir    <- getAppDir
        startTime <- getPOSIXTime
        writeFile (appDir ++ "/time") $ show startTime ++ "\n"

-- | Track an Issue By Writing It's ID to a File
trackIssue :: Integer -> IO ()
trackIssue issueID      = do
        appDir <- getAppDir
        writeFile (appDir ++ "/issue") $ show issueID ++ "\n"


-- Utils

-- | Retrieve the App's User Data Directory
getAppDir :: IO FilePath
getAppDir = getAppUserDataDirectory "hkredmine"
