{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This module contains the actions that are run by the CLI dispatcher.
-
-}

module Main.Actions
        ( printStatus
        , printFields
        , printProject
        , printProjects
        , printIssue
        , printIssues
        , printVersion
        , printVersions
        , printNextVersion
        , createNewIssue
        , closeIssue
        , switchAccount
        , startTimeTracking
        , stopTimeTracking
        , pauseTimeTracking
        , resumeTimeTracking
        , abortTimeTracking
        , watchIssue
        , unwatchIssue
        ) where


import qualified Data.ByteString.Lazy as LB     (ByteString)
import qualified Data.List as L

import Control.Applicative      ((<$>))
import Control.Monad            (when, unless, join, void)
import Control.Monad.Except     (runExceptT)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.=), object, encode)
import Data.ConfigFile          (emptyCP, readfile, has_section)
import Data.Function            (on)
import Data.Time.Clock          (getCurrentTime, utctDay,
                                 secondsToDiffTime, DiffTime)
import Data.Time.Clock.POSIX    (getPOSIXTime)
import Data.Maybe               (fromJust, isJust, isNothing)
import System.Time.Utils        (renderSecs)
import System.Exit              (exitFailure)
import System.Directory         (getHomeDirectory)
import Text.Printf              (printf)

import Web.HTTP.Redmine

import Main.Utils


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

-- | Print the available Statuses, Trackers, Priorities and Time Entry
-- Activities.
printFields :: Redmine ()
printFields             =
        getStatuses >>= showFields "Issue Statuses" . map statusName >>
        getTrackers >>= showFields "Issue Trackers" . map trackerName >>
        getPriorities >>= showFields "Issue Priorities" . map priorityName >>
        getActivities >>= showFields "Time Entry Activities" . map activityName
        where showFields name fields = liftIO
                                     $ putStrLn (name ++ ":")
                                    >> mapM_ putStrLn fields
                                    >> putStrLn ""

-- | Print All Projects
printProjects :: Redmine ()
printProjects           = getProjects >>= liftIO . putStrLn . projectsTable

-- | Print A single 'Project'.
printProject :: ProjectIdent -> Redmine ()
printProject pIdent     = getProjectFromIdent pIdent >>=
                          liftIO . putStrLn . projectDetail

-- | Print Issues filtered by a command line arguments
printIssues :: IssueFilter -> Redmine ()
printIssues f           = getIssues f >>= liftIO . putStrLn . issuesTable

-- | Print a single 'Issue'.
printIssue :: IssueId -> Redmine ()
printIssue i            = getIssue i >>= liftIO . putStrLn . issueDetail


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


-- Issue Creation/Updates
-- | Create a new Issue and print it's ID.
createNewIssue :: LB.ByteString -> Redmine ()
createNewIssue postData = do
        issue           <- createIssue postData
        liftIO . putStrLn $ "Created Issue #" ++ show (issueId issue) ++ ": " ++
                            issueSubject issue

-- | Close an Issue, set it's Done Ratio to 100 and if it's Due Date is not
-- set, set it to today.
closeIssue :: IssueId -> Redmine ()
closeIssue i            = do
        liftIO . putStrLn $ "Closing Issue #" ++ show i ++ "..."
        issue           <- getIssue i
        closedStatus    <- getStatusFromName "Closed"
        today           <- liftIO $ fmap utctDay getCurrentTime
        let updateDue   = isNothing (issueDueDate issue) ||
                          fromJust (issueDueDate issue) `notElem` [ "", show today ]
            updateStat  = isJust closedStatus && issueStatus issue /= "Closed"
            updateDone  = issueDoneRatio issue /= 100
            doSomething = updateStat || updateDue || updateDone
            putData     = encode $ object [ "issue" .= object (concat
                        [ [ "status_id" .= statusId (fromJust closedStatus)
                                | updateStat ]
                        , [ "due_date" .= show today
                                | updateDue ]
                        , [ "done_ratio" .= (100 :: Integer)
                                | updateDone ]
                        , [ "notes" .= ("Closing Issue." :: String)
                                | doSomething ]
                        ]) ]
        void $ updateIssue i putData
        whenPrint updateStat "Issue status changed to Closed."
        whenPrint updateDue "Issue due date set to today."
        whenPrint updateDone "Issue done ratio set to 100%."
        whenPrint (not doSomething) "Found nothing to do."
        where whenPrint b s     = when b (liftIO $ putStrLn s)


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

-- | Stop time tracking for an 'Issue' by calculating the time spent,
-- prompting for input and submiting the entry.
stopTimeTracking :: Maybe String -> Maybe String -> Redmine ()
stopTimeTracking mayActivity mayComment = do
        timeSpent   <- liftIO getTrackedTime
        issue       <- liftIO getTrackedIssue
        activities  <- getActivities
        let activityNames = map activityName activities
            activityIds   = map (show . activityId) activities
            validActivity = (`elem` activityNames ++ activityIds)
        activity    <- if isJust mayActivity && validActivity (fromJust mayActivity)
                       then return $ fromJust mayActivity
                       else liftIO $ getUntilValidWithInfo validActivity
                                     "Enter a valid activity:"
                                     ("\nValid Time Entry Activities:":activityNames)
        comment     <- maybe (liftIO $ putStrLn "\nEnter a short comment: " >> getLine)
                             return mayComment
        confirmed   <- liftIO . confirmWithInfo $
                            [ "\nCreate the following Time Entry?"
                            , "Activity:\t" ++ activity
                            , "Comment: \t" ++ comment
                            , "Hours:   \t" ++ printf "%.2f" (diffTimeToHours timeSpent)
                            ]
        timeActivity <- fromJust <$> getActivityFromName activity
        if   confirmed
        then addTimeEntry issue timeSpent timeActivity comment >>
             liftIO (mapM_ removeAppFile [ "issue", "start_time", "pause_time" ]) >>
             liftIO (putStrLn "The time entry was successfully created.")
        else redmineLeft "Time entry submission cancelled."

-- | Pause the Time Tracking by writing the current POSIX time to the
-- `pause_time` file.
pauseTimeTracking :: IO ()
pauseTimeTracking       = do
        _               <- readFileOrExit "start_time"
                                "Can't pause, not currently tracking time."
        paused          <- appFileExists "pause_time"
        when paused $ putStrLn "Time tracking is already paused." >> exitFailure
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


-- Utils
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


-- | Write some text then ask for a string until a valid one is given.
getUntilValidWithInfo :: (String -> Bool) -> String -> [String] -> IO String
getUntilValidWithInfo p prompt info = mapM_ putStrLn info
                                   >> untilValid p (putStrLn ("\n" ++ prompt) >>
                                                    getLine)

-- | Write some text then ask for a confirmation.
confirmWithInfo :: [String] -> IO Bool
confirmWithInfo info    = do mapM_ putStrLn info >> putStrLn "\nConfirm(y/n): "
                             response <- getLine
                             return $ response `elem` [ "y", "Y", "yes"
                                                      , "Yes", "YES" ]

-- | Repeat an IO action until the value inside returns True for some
-- predicate.
untilValid :: (a -> Bool) -> IO a -> IO a
untilValid p action     = do result <- action
                             if p result then return result else untilValid p action

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
