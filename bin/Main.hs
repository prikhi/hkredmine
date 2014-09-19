{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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

import qualified Data.ByteString.Lazy.Char8 as LC    (unpack)

import Control.Applicative      ((<$>))
import Control.Monad            (void, when, unless)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.=), object, encode)
import Data.Time.Clock          (getCurrentTime, utctDay)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Maybe               (fromJust, isJust)
import System.Environment       (getArgs)
import System.Exit              (exitFailure, exitSuccess)
import System.Directory         (createDirectoryIfMissing, removeFile,
                                 doesFileExist)

import Web.HTTP.Redmine

import Main.Utils

-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main              = do
        _       <- initializeApp
        args    <- getArgs
        defCfg  <- defaultRedmineConfig
        let cfg = defCfg { redURL = ""
                         , redAPI = ""
                         }
        result  <- runRedmine cfg $ commandHandler args
        case result of
            Right r  -> putStr   "OK:    " >> print r >> exitSuccess
            Left err -> putStrLn "ERROR: " >> putStrLn err >> exitFailure

commandHandler :: [String] -> Redmine ()
commandHandler args = case args of
        ["projects"]                    -> printProjects
        ["print", "project", projectID] -> printProject $ read projectID
        ["print", "issues"]             -> printIssues
        ["details", _      ]            -> error "Not Yet Implemented"
        ["track", "project", projectID] -> liftIO . trackProject $ read projectID
        ["startwork", issueID]          -> startTimeTracking $ read issueID
        ["pause"]                       -> liftIO pauseTimeTracking
        ["resume"]                      -> liftIO resumeTimeTracking
        ["stopwork"]                    -> error "Not Yet Implemented"
        _                               -> liftIO printUsage

-- | Create the Data Directory & Files for the Application
initializeApp :: IO ()
initializeApp = getAppDir >>=
                createDirectoryIfMissing True

-- | Print the Program's Usage Text
printUsage :: IO ()
printUsage        =
    let message =
            [ "HKRedmine - Redmine CLI Client"
            , ""
            , "Usage:"
            , "hkredmine command <args> --<param>=<value>"
            , ""
            , "Commands:"
            , "projects                   -- Print All Projects"
            , "print project <id>         -- Print the Details of a Specific Project"
            , "print issues               -- Print All Issues of the Tracked Project"
            , "print myissues             -- Print Your Issues of the Tracked Project"
            , "track project <id>         -- Track the Specified Project"
            , "startwork <id>             -- Start Tracking Time for an Issue"
            , "pause                      -- Pause Time Tracking"
            , "resume                     -- Resume Tracking Time(only if paused)"
            , "stopwork                   -- Stop Tracking Time & Submit an Entry"
            , ""
            ]
    in mapM_ putStrLn message


-- Displaying Data

-- | Print All Projects
printProjects :: Redmine ()
printProjects           = do Projects ps <- getProjects
                             liftIO . putStrLn . projectsTable $ ps

-- | Print A Single Project
printProject :: ProjectId -> Redmine ()
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

-- Project Tracking
-- | Track a Project by writing it's ID to a File
trackProject :: ProjectId -> IO ()
trackProject projectID  = writeAppFile "project" $ show projectID ++ "\n"

-- | Retrieve the currently tracked Project
getTrackedProject :: IO ProjectId
getTrackedProject       = do
        projectID <- readAppFile "project"
        return $ read projectID

-- Issue Tracking
-- | Track an Issue by writing it's ID to a File
trackIssue :: IssueId -> IO ()
trackIssue issueID      = do
        writeAppFile "issue" $ show issueID ++ "\n"

-- | Retrieve the currently tracked Issue
getTrackedIssue :: IO IssueId
getTrackedIssue         = do
        issueID   <- readAppFile "issue"
        return $ read issueID

-- Time Tracking
-- | Initiate time tracking by writing the current POSIX time to the
-- `start_time` file.
startTimeTracking :: IssueId -> Redmine ()
startTimeTracking i     = do
        appDir          <- liftIO $ getAppDir
        alreadyTracking <- liftIO $ doesFileExist $ appDir ++ "/start_time"
        if   alreadyTracking
        then liftIO $ putStrLn ("Can't start, we're already tracking time for " ++
                                "an issue.") >> exitFailure
        else markAsInProgressAndSetStartDate i >>
             liftIO (trackIssue i >> writeTimeFile "start_time" >>
                     putStrLn ("Time Tracking Started on Issue #" ++ show i ++ "."))

-- | Mark an 'Issue' as "In Progress" if the current 'Status' is the
-- default status and set the 'issueStartDate' to today if it is unset.
markAsInProgressAndSetStartDate :: IssueId -> Redmine ()
markAsInProgressAndSetStartDate i   = do
        issue           <- getIssue i
        let setStartDate = issueStartDate issue == Nothing
        status          <- fmap fromJust . getStatusFromName . issueStatus $ issue
        maybeInProgress <- getStatusFromName "In Progress"
        let changeStatus = statusIsDefault status && isJust maybeInProgress
            notes        = if changeStatus || setStartDate
                           then "Starting work on this issue." else "" :: String
        today           <- liftIO $ fmap utctDay getCurrentTime
        let putData      = LC.unpack . encode $ object [ "issue" .= (object $
                concat [ ["status_id" .= (statusId . fromJust $ maybeInProgress)
                                | changeStatus]
                        , ["start_date" .= show today
                                | setStartDate]
                        , ["notes" .= notes]
                        ] ) ]
        unless (isJust maybeInProgress)
               (liftIO . putStrLn $ "Issue status is unchanged because we couldn't "
                                 ++ "find an 'In Progress' status.")
        when (changeStatus || setStartDate) (updateIssue i putData)
        when (changeStatus) (liftIO . putStrLn $ "Changed the Issue Status to "
                                              ++ "'In Progress'.")
        when (setStartDate) (liftIO . putStrLn $ "Set the Start Date to today.")

-- | Pause the Time Tracking by writing the current POSIX time to the
-- `pause_time` file.
pauseTimeTracking :: IO ()
pauseTimeTracking       = readFileOrExit "start_time"
                                "Can't pause, not currently tracking time." >>
                          writeTimeFile "pause_time" >>
                          putStrLn "Time Tracking Paused."

-- | Resume paused time tracking by reading the `start_time` and
-- `pause_time` files and then calculating and writing the new
-- `start_time`.
resumeTimeTracking :: IO ()
resumeTimeTracking      = do
        appDir      <- getAppDir
        startTime   <- read <$> readFileOrExit "start_time"
                                               "No previous time tracking to resume."
        pauseTime   <- read <$> readFileOrExit "pause_time"
                                               "Time tracking isn't paused."
        currentTime <- fmap round getPOSIXTime :: IO Integer
        let newStartTime = currentTime - (pauseTime - startTime)
        writeAppFile "start_time" $ show newStartTime ++ "\n"
        removeFile $ appDir ++ "/pause_time"
        putStrLn "Time Tracking Resumed."
