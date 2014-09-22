{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : Main
Description     : A Redmine CLI Client
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX
-}
module Main (main) where

import qualified Data.List as L

import Control.Applicative      ((<$>))
import Control.Monad            (when, unless, join)
import Control.Monad.Except     (runExceptT)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.=), object, encode)
import Data.ConfigFile          (get, emptyCP, readfile, has_section, sections,
                                 CPErrorData(..))
import Data.Function            (on)
import Data.Time.Clock          (getCurrentTime, utctDay,
                                 secondsToDiffTime, DiffTime)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Maybe               (fromJust, isJust, isNothing)
import System.Time.Utils        (renderSecs)
import System.Environment       (getArgs)
import System.Exit              (exitFailure, exitSuccess)
import System.Directory         (createDirectoryIfMissing, getHomeDirectory)

import Web.HTTP.Redmine

import Main.Utils

-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main                    = do
        cfg             <- initializeApp
        args            <- getArgs
        result          <- runRedmine cfg $ commandHandler args
        case result of
            Right r     -> putStr "OK: " >> print r >> exitSuccess
            Left err    -> putStr "ERROR: " >> putStrLn err >> exitFailure

commandHandler :: [String] -> Redmine ()
commandHandler args     = case args of
        ["use", accountName]            -> liftIO $ switchAccount accountName
        ["status"]                      -> liftIO printStatus
        ["projects"]                    -> printProjects
        ["project", projectIdent]       -> printProject projectIdent
        ["issues", projectIdent]        -> printProjectsIssues projectIdent
        ["details", _      ]            -> error "Not Yet Implemented"
        ["startwork", issueID]          -> startTimeTracking $ read issueID
        ["pause"]                       -> liftIO pauseTimeTracking
        ["resume"]                      -> liftIO resumeTimeTracking
        ["stopwork"]                    -> error "Not Yet Implemented"
        ["abort"]                       -> liftIO abortTimeTracking
        ["watch", issueID]              -> watchIssue $ read issueID
        ["unwatch", issueID]            -> unwatchIssue $ read issueID
        ["versions", projectIdent]      -> printVersions projectIdent
        ["version", versionID]          -> printVersion $ read versionID
        ["nextversion", projectIdent]   -> printNextVersion projectIdent
        _                               -> liftIO printUsage

-- | Create the data directory for the application, read the config file
-- and return a 'RedmineConfig'.
initializeApp :: IO RedmineConfig
initializeApp           = do
        appDir  <- getAppDir
        _       <- createDirectoryIfMissing True appDir
        getConfig

-- | Read the User's config file and attempt to generate a 'RedmineConfig'.
getConfig :: IO RedmineConfig
getConfig               = do
        defCfg  <- defaultRedmineConfig
        homeDir <- getHomeDirectory
        let configPath = homeDir ++ "/.hkredminerc"
        mayAcc  <- getAccount
        eithCfg <- runExceptT $ do
            cp  <- join $ liftIO $ readfile emptyCP configPath
            let acc = if isJust mayAcc && has_section cp (fromJust mayAcc)
                then fromJust mayAcc
                else head $ sections cp  ++ ["DEFAULT"]
            url <- get cp acc "url"
            key <- get cp acc "apikey"
            return $ defCfg { redURL = url, redAPI = key }
        case eithCfg of
            Right cfg               -> return cfg
            Left (NoOption o, _)    -> putStrLn ("Could not read " ++ o ++ " Option.")
                                    >> exitFailure
            Left (NoSection s, _)   -> putStrLn ("Could not find a " ++ s ++ " Account.")
                                    >> exitFailure
            Left (ParseError _, _)  -> putStrLn "Could not parse config file."
                                    >> exitFailure
            Left _                  -> putStrLn "Encountered an error reading the config file."
                                    >> exitFailure

-- | Print the Program's Usage Text
printUsage :: IO ()
printUsage              =
    let message         =
            [ ""
            , "HKRedmine - Redmine CLI Client"
            , ""
            , ""
            , "Usage:"
            , "hkredmine command <args> --<param>=<value>"
            , ""
            , ""
            , "Commands:"
            , ""
            , "use <account>                    -- Switch to a Different Redmine Account"
            , "status                           -- Print the Current Account, Issue and Time"
            , ""
            , "-- Projects"
            , "projects                         -- Print All Projects"
            , "project <project_ident>          -- Print the Details of a Specific Project"
            , ""
            , "-- Issues"
            , "issues <project_ident>           -- Print All Issues of the Tracked Project"
            , "myissues <project_ident>         -- Print Your Issues of the Tracked Project"
            , ""
            , "-- Time Tracking"
            , "startwork <issue_id>             -- Start Tracking Time for an Issue"
            , "pause                            -- Pause Time Tracking"
            , "resume                           -- Resume Tracking Time(only if paused)"
            , "stopwork                         -- Stop Tracking Time & Submit an Entry"
            , "abort                            -- Abort Time Tracking"
            , ""
            , "-- Watching"
            , "watch <issue_id>                 -- Watch an Issue"
            , "unwatch <issue_id>               -- Unwatch an Issue"
            , ""
            , "-- Versions"
            , "versions <project_ident>         -- Print All of a Project's Versions"
            , "version <version_id>             -- Print a Version"
            , "nextversion <project_ident>      -- Print a Project's Next Due Version"
            , ""
            ]
    in mapM_ putStrLn message


-- Displaying Data
--
-- | Print the current account, issue and time, if any.
printStatus :: IO ()
printStatus             = do
        mayAccount  <- getAccount
        tracking    <- appFileExists "issue"
        issue       <- if tracking then getTrackedIssue else return 0
        started     <- appFileExists "start_time"
        paused      <- appFileExists "pause_time"
        trackedTime <- if tracking then getTrackedTime else return 0
        mapM_ putStrLn  $
            [ "Using account \"" ++ fromJust mayAccount ++ "\"."
                    | isJust mayAccount ]
         ++ [ "Currently tracking time for Issue #" ++ show issue ++ "."
                    | tracking && started && not paused ]
         ++ [ "Time tracking for Issue #" ++ show issue ++ " is currently paused."
                    | tracking && started && paused ]
         ++ [ "You have tracked " ++ renderSecs (round trackedTime) ++ " on this Issue so far."
                    | tracking ]
         ++ [ "Not currenlty tracking an issue or an account."
                    | not tracking && isNothing mayAccount ]


-- | Calculate the current amount of time tracked.
getTrackedTime :: IO DiffTime
getTrackedTime          = do
        start_time      <- read <$> readAppFile "start_time"
        paused          <- appFileExists "pause_time"
        pause_time      <- if paused then read <$> readAppFile "pause_time" else return 0
        current_time    <- round <$> getPOSIXTime
        let trackedSecs = if paused then pause_time - start_time
                                    else current_time - start_time
        return $ secondsToDiffTime trackedSecs


-- | Print All Projects
printProjects :: Redmine ()
printProjects           = getProjects >>= liftIO . putStrLn . projectsTable

-- | Print A Single 'Project'.
printProject :: ProjectIdent -> Redmine ()
printProject pIdent     = getProjectFromIdent pIdent >>=
                          liftIO . putStrLn . projectDetail

-- | Print All Issues of a 'Project'.
printProjectsIssues :: ProjectIdent -> Redmine ()
printProjectsIssues pIdent      = do
        proj        <- getProjectFromIdent pIdent
        issues      <- getProjectsIssues (projectId proj) []
        liftIO . putStrLn . issuesTable $ issues

-- | Print A 'Version' and it's Issues.
printVersion :: VersionId -> Redmine ()
printVersion v          = do
        version <- getVersion v
        let pID = versionProjectId version
        issues  <- getVersionsIssues pID version
        liftIO $ putStrLn (versionDetail version) >> putStrLn "" >>
                 putStrLn "Issues:" >> putStrLn (issuesTable issues)

-- | Print a table showing all 'Versions' of a 'Project'.
printVersions :: ProjectIdent -> Redmine ()
printVersions pIdent    = do
        versions <- getProjectFromIdent pIdent >>= getVersions . projectId
        let sortedVs = L.sortBy (flip compare `on` versionDueDate) versions
        liftIO . putStrLn . versionTable $ sortedVs

-- | Print the 'Version' that is next due for a 'Project'.
printNextVersion :: ProjectIdent -> Redmine ()
printNextVersion pIdent = do
        maybeVersion    <- getProjectFromIdent pIdent >>=
                           getNextVersionDue . projectId
        case maybeVersion of
            Nothing     -> redmineLeft "No valid version found."
            Just v      -> printVersion $ versionId v

-- Account Tracking
-- | Switch the current account used.
switchAccount :: String -> IO ()
switchAccount account   = do
        homeDir <- getHomeDirectory
        let configPath = homeDir ++ "/.hkredminerc"
        eithAcc <- runExceptT $ do
            cp  <- join $ liftIO $ readfile emptyCP configPath
            return $ has_section cp account
        case eithAcc of
            Right b   -> if b then writeAppFile "account" account
                                >> putStrLn ("Successfully switched to " ++
                                             account ++ ".")
                              else exitError "Account not found in config file."
            Left _    -> exitError "Could not parse config file."
        where exitError msg = putStrLn msg >> exitFailure

-- | Retrieve the currently used account, or the string  `DEFAULT`
getAccount :: IO (Maybe String)
getAccount              = do
        tracking        <- appFileExists "account"
        if tracking
            then Just <$> readAppFile "account"
            else return Nothing

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
        alreadyTracking <- liftIO $ appFileExists "start_time"
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
        let putData      = encode $ object [ "issue" .= object (concat
                    [ ["status_id" .= (statusId . fromJust $ maybeInProgress)
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
        paused          <- appFileExists "pause_time"
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


-- Watching
-- | Watch an 'Issue' as the current 'User'.
watchIssue :: IssueId -> Redmine ()
watchIssue i            = getCurrentUser >>= addWatcher i >>
                          (liftIO . putStrLn $ "Started watching Issue #" ++
                                               show i ++ ".")

-- | Unwatch an 'Issue' as the current 'User'.
unwatchIssue :: IssueId -> Redmine ()
unwatchIssue i          = getCurrentUser >>= removeWatcher i >>
                          (liftIO . putStrLn $ "Stopped watching Issue #" ++
                                               show i ++ ".")
