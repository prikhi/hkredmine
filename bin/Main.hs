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

import qualified Data.ByteString.Lazy.Char8 as LC    (unpack)

import Control.Applicative      ((<$>))
import Control.Monad            (void, when, unless)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.=), object, encode)
import Data.Time.Clock          (getCurrentTime, utctDay)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Maybe               (fromJust, isJust, isNothing)
import System.Environment       (getArgs)
import System.Exit              (exitFailure, exitSuccess)
import System.Directory         (createDirectoryIfMissing, doesFileExist)

import Web.HTTP.Redmine

import Main.Utils

-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main                    = do
        _               <- initializeApp
        args            <- getArgs
        defCfg          <- defaultRedmineConfig
        let cfg         = defCfg
               { redURL = ""
               , redAPI = "" }
        result          <- runRedmine cfg $ commandHandler args
        case result of
            Right r     -> putStr "OK: " >> print r >> exitSuccess
            Left err    -> putStr "ERROR: " >> putStrLn err >> exitFailure

commandHandler :: [String] -> Redmine ()
commandHandler args     = case args of
        ["projects"]                    -> printProjects
        ["print", "project", projectID] -> printProject $ read projectID
        ["print", "issues"]             -> printIssues
        ["details", _      ]            -> error "Not Yet Implemented"
        ["track", "project", projectID] -> liftIO . trackProject $ read projectID
        ["startwork", issueID]          -> startTimeTracking $ read issueID
        ["pause"]                       -> liftIO pauseTimeTracking
        ["resume"]                      -> liftIO resumeTimeTracking
        ["stopwork"]                    -> error "Not Yet Implemented"
        ["abort"]                       -> liftIO abortTimeTracking
        _                               -> liftIO printUsage

-- | Create the Data Directory & Files for the Application
initializeApp :: IO ()
initializeApp           = getAppDir >>= createDirectoryIfMissing True

-- | Print the Program's Usage Text
printUsage :: IO ()
printUsage              =
    let message         =
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
            , "abort                      -- Abort Time Tracking"
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
trackProject projectID  = writeAppFile "project" $ show projectID

-- | Retrieve the currently tracked Project
getTrackedProject :: IO ProjectId
getTrackedProject       = read <$> readAppFile "project"

-- Issue Tracking
-- | Track an Issue by writing it's ID to a File
trackIssue :: IssueId -> IO ()
trackIssue issueID      = writeAppFile "issue" $ show issueID

-- | Retrieve the currently tracked Issue
getTrackedIssue :: IO IssueId
getTrackedIssue         = read <$> readAppFile "issue"

-- Time Tracking
-- | Initiate time tracking for an 'Issue' by writing the current POSIX
-- time to the `start_time` file.
--
-- This includes changing the Status to `In Progress` setting  if the
-- 'Issue' has the default 'Status' and setting the 'issueStartDate' to the
-- current date(if it's not set).
startTimeTracking :: IssueId -> Redmine ()
startTimeTracking i     = do
        appDir          <- liftIO getAppDir
        alreadyTracking <- liftIO $ doesFileExist $ appDir ++ "start_time"
        if   alreadyTracking
        then do trackedID <- liftIO getTrackedIssue
                redmineLeft $ "Can't start, we're already tracking time for "
                           ++ "Issue #" ++ show trackedID ++ "."
        else liftIO (trackIssue i >> writeTimeFile "start_time" >>
                     putStrLn ("Started tracking time for Issue #" ++ show i ++ "."))
          >> markAsInProgressAndSetStartDate i

-- | Mark an 'Issue' as "In Progress" if the current 'Status' is the
-- default status and set the 'issueStartDate' to today if it is unset.
markAsInProgressAndSetStartDate :: IssueId -> Redmine ()
markAsInProgressAndSetStartDate i   = do
        issue           <- getIssue i
        status          <- fmap fromJust . getStatusFromName . issueStatus $ issue
        maybeInProgress <- getStatusFromName "In Progress"
        let changeStatus = statusIsDefault status && isJust maybeInProgress
            setStartDate = isNothing . issueStartDate $ issue
            notes        = if changeStatus || setStartDate
                           then "Starting work on this issue." else "" :: String
        today           <- liftIO $ fmap utctDay getCurrentTime
        let putData      = LC.unpack . encode $ object [ "issue" .= object
                (concat [ ["status_id" .= (statusId . fromJust $ maybeInProgress)
                                | changeStatus]
                        , ["start_date" .= show today
                                | setStartDate]
                        , ["notes" .= notes]
                        ] ) ]
        unless (isJust maybeInProgress)
               (liftIO . putStrLn $ "Issue status is unchanged because we couldn't "
                                 ++ "find an 'In Progress' status.")
        when (changeStatus || setStartDate) (updateIssue i putData)
        when changeStatus (liftIO . putStrLn $ "Changed the Issue Status to "
                                            ++ "'In Progress'.")
        when setStartDate (liftIO . putStrLn $ "Set the Start Date to today.")

-- | Pause the Time Tracking by writing the current POSIX time to the
-- `pause_time` file.
pauseTimeTracking :: IO ()
pauseTimeTracking       = do
        _               <- readFileOrExit "start_time"
                                "Can't pause, not currently tracking time."
        appDir          <- getAppDir
        paused          <- doesFileExist $ appDir ++ "pause_time"
        when paused $ putStrLn "Time tracking is already paused." >> exitSuccess
        _               <- writeTimeFile "pause_time"
        putStrLn "Paused time tracking."

-- | Resume paused time tracking by reading the `start_time` and
-- `pause_time` files and then calculating and writing the new
-- `start_time`.
resumeTimeTracking :: IO ()
resumeTimeTracking      = do
        startTime       <- readFileOrExit "start_time"
                                          "No previous time tracking to resume."
        pauseTime       <- readFileOrExit "pause_time"
                                          "Time tracking isn't paused."
        currentTime     <- round <$> getPOSIXTime :: IO Integer
        let newStartTime = currentTime - (read pauseTime - read startTime)
        removeAppFile "pause_time"
        writeAppFile "start_time" $ show newStartTime
        trackedID       <- getTrackedIssue
        putStrLn $ "Resumed tracking time for Issue #" ++ show trackedID ++ "."

-- | Abort time tracking by removing the `issue`, `start_time` and
-- `pause_time` files if they exist.
abortTimeTracking :: IO ()
abortTimeTracking       = mapM_ removeAppFile [ "issue", "start_time", "pause_time" ]
                       >> putStrLn "Aborted time tracking."
