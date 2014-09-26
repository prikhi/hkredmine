{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
-
- This module contains functions related to CLI arguement parsing and
- dispatch.
-
-}
module Main.CLI
        ( hkredmine
        , dispatch
        ) where

import qualified Web.HTTP.Redmine as R
import qualified Data.ByteString.Char8 as BC    (pack)
import qualified Data.ByteString.Lazy as LB     (ByteString)


import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.=), object, encode, FromJSON)
import Data.Maybe               (isNothing, fromJust)
import System.Console.CmdArgs
import System.Directory         (removeFile)
import System.Environment       (getEnv)
import System.IO                (hClose)
import System.IO.Temp           (withSystemTempDirectory, withTempFile)
import System.Process           (callProcess)

import Web.HTTP.Redmine         (Redmine, IssueFilter, redmineLeft)

import Main.Actions

-- | Usage modes options for the CLI interface.
data HKRedmine
        = Use           { accountName       :: String }
        | Status        { }
        | Fields        { }
        | Projects      { }
        | Project       { projectIdent      :: String }
        | Issue         { issueId           :: Integer }
        | Issues        { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
        | NewIssue      { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , subject           :: String
                        , description       :: String
                        , versionId         :: Integer
                        , editDescript      :: Bool
                        , isNotMine         :: Bool }
        | Close         { issueId           :: Integer }
        | StartWork     { issueId           :: Integer }
        | StopWork      { activityType      :: Maybe String
                        , timeComment       :: Maybe String }
        | Pause         { }
        | Resume        { }
        | Abort         { }
        | Watch         { issueId           :: Integer }
        | Unwatch       { issueId           :: Integer }
        | Versions      { projectIdent      :: String }
        | Version       { versionId         :: Integer }
        | NextVersion   { projectIdent      :: String }
         deriving (Show, Data, Typeable)


-- | Route a 'HKRedmine' Mode to a 'Redmine' Action.
dispatch :: HKRedmine -> Redmine ()
dispatch m = case m of
        Use a           -> liftIO $ switchAccount a
        Status          -> liftIO printStatus
        Fields          -> printFields
        Projects        -> printProjects
        Project p       -> printProject p
        Issue i         -> printIssue i
        i@(Issues {})   -> argsToIssueFilter i >>= printIssues
        i@(NewIssue {}) -> argsToIssueObject i >>= createNewIssue
        Close i         -> closeIssue i
        StartWork i     -> startTimeTracking i
        StopWork a c    -> stopTimeTracking a c
        Pause           -> liftIO pauseTimeTracking
        Resume          -> liftIO resumeTimeTracking
        Abort           -> liftIO abortTimeTracking
        Watch i         -> watchIssue i
        Unwatch i       -> unwatchIssue i
        Versions p      -> printVersions p
        Version v       -> printVersion v
        NextVersion p   -> printNextVersion p


-- | Available usage modes
hkredmine :: Annotate Ann
hkredmine = modes_
        [ use, status, fields
        , project, projects
        , issue, issues, newissue, close
        , startwork, stopwork, pause, resume, abort
        , watch, unwatch
        , version, versions, nextversion ]
        += help "A Redmine CLI client"
        += program "hkredmine"
        += summary "HKRedmine v0.1"


-- | Default options for modes
use, status, fields, projects, project, issue, issues, newissue, close,
    startwork, stopwork, pause, resume, abort, watch, unwatch, versions,
    version, nextversion :: Annotate Ann
use         = record Use { accountName = def }
            [ accountName := def
                          += argPos 0
                          += typ "ACCOUNTNAME"
            ] += help "Switch to a different redmine account."
              += details
    [ "Multiple accounts are available by adding a \"[AccountName]\" line"
    , "before each account's \"apikey\" and \"url\" in your ~/.hkredminerc:"
    , ""
    , "[account1]"
    , "apikey = \"longalphanumericstring\""
    , "url = http://redmine.yourdomain.com/"
    , ""
    , "[account2]"
    , "apikey = \"differentkey\""
    , "url = http://redmine.otherdomain.com/"
    , ""
    , "Then you can use the \"use\" command to switch to the second account:"
    , "hkredmine use account2"
    ]
status      = record Status {} []
              += help "Print the current Account, Issue and Tracked Time."
              += auto

fields      = record Fields {} []
              += help "Print the available field values(Statuses, Priorities, etc.)."

projects    = record Projects {} []
              += help "Print all Projects."
              += groupname "Projects"

project     = record Project { projectIdent = def }
            [ projectIdent  := def
                            += argPos 0 += typ "PROJECTIDENT"
            ] += help "Print the details of a Project."
              += groupname "Projects"

issue       = record Issue { issueId = def }
            [ issueId       := def
                            += argPos 0 += typ "ISSUEID"
            ] += help "Print the details of an Issue."
              += groupname "Issues"

issues      = record Issues { projectIdent = def, statusIdent = def
                            , trackerIdent = def, priorityIdent = def
                            , assignedTo = def, sortByField = def
                            , limitTo = def, issueOffset = def }
            [ projectIdent  := def
                            += typ "PROJECTIDENT"
                            += name "project"
                            += name "p"
                            += groupname "filter"
                            += explicit
                            += help "A Project's Identifier or ID"
            , statusIdent   := "open"
                            += typ "STATUS"
                            += name "status"
                            += name "s"
                            += groupname "filter"
                            += explicit
                            += help "A Status Name. open and closed are also valid choices"
            , trackerIdent  := def
                            += typ "TRACKERNAME"
                            += name "tracker"
                            += name "t"
                            += groupname "filter"
                            += explicit
                            += help "A Tracker Name"
            , priorityIdent := def
                            += typ "PRIORITYNAME"
                            += name "priority"
                            += name "i"
                            += groupname "filter"
                            += explicit
                            += help "A Priority Name"
            , assignedTo    := ""
                            += opt ("me" :: String)
                            += name "userid"
                            += name "u"
                            += groupname "filter"
                            += explicit
                            += help "A User ID(defaults to yours)"
            , issueOffset   := 0
                            += typ "INT"
                            += name "offset"
                            += name "o"
                            += explicit
                            += groupname "range"
                            += help "Start listing Issues at this row number"
            , sortByField   := "updated_on"
                            += typ "FIELD"
                            += name "sort"
                            += name "S"
                            += explicit
                            += groupname "sort"
                            += help ("Comma-separated columns to sort by. " ++
                                     "Valid options are project, priority, " ++
                                     "status, category, updated_on and " ++
                                     "created_on" )
            , limitTo       := 20
                            += typ "INT"
                            += name "limit"
                            += name "n"
                            += name "l"
                            += explicit
                            += groupname "range"
                            += help "Limit the number of Issues to show"
            ] += help "Filter and Print Issues."
              += groupname "Issues"
              += details
    [ "Run `hkredmine fields` for valid Status, Priority and"
    , "Tracker values."
    , ""
    , "Example Usage:"
    , "hkredmine issues -u"
    , "hkredmine issues -n 50 -p accounting-app -S priority"
    , "hkredmine issues --status=open -t Bug"
    ]

newissue    = record NewIssue { projectIdent = def, trackerIdent = def
                              , statusIdent = def, priorityIdent = def
                              , isNotMine = def, subject = def
                              , description = def, versionId = def
                              , editDescript = False }
            [ projectIdent  := def
                            += typ "PROJECTIDENT"
                            += name "project"
                            += name "p"
                            += groupname "Required"
                            += explicit
                            += help "A Project's Identifier or ID"
            , subject       := def
                            += typ "STRING"
                            += name "subject"
                            += name "s"
                            += groupname "Required"
                            += explicit
                            += help "The Issue's Subject"
            , description   := def
                            += typ "STRING"
                            += name "description"
                            += name "d"
                            += explicit
                            += groupname "Optional"
                            += help "A Full Description About the Issue"
            , statusIdent   := def
                            += typ "STATUSNAME"
                            += name "status"
                            += name "a"
                            += explicit
                            += groupname "Optional"
                            += help "A Status Name"
            , trackerIdent  := def
                            += typ "TRACKERNAME"
                            += name "tracker"
                            += name "t"
                            += explicit
                            += groupname "Optional"
                            += help "A Tracker Name"
            , priorityIdent := def
                            += typ "PRIORITYNAME"
                            += name "priority"
                            += name "i"
                            += explicit
                            += groupname "Optional"
                            += help "A Priority Name"
            , isNotMine     := def
                            += name "not-mine"
                            += name "n"
                            += explicit
                            += groupname "Optional"
                            += help "Don't assign it to me"
            , versionId     := def
                            += typ "INT"
                            += name "vers"
                            += name "v"
                            += explicit
                            += groupname "Optional"
                            += help "A Version's ID"
            , editDescript  := def
                            += name "e"
                            += name "edit-description"
                            += explicit
                            += help "Edit the description in $EDITOR. Ignores -d"
            ] += help "Create a New Issue."
              += groupname "Issues"
              += details
    [ "A minimum of a project and status are required to create an Issue:"
    , "hkredmine newissue -p my-project -s 'Do Some Things'"
    , ""
    , "You can also write the description in your $EDITOR by setting the '-e' flag:"
    , "hkredmine newissue -p my-project -s 'Test Writing Description in Vim' -e"
    , ""
    , "This may not work correctly if your $EDITOR runs asynchronously. Try"
    , "using a CLI text editor:"
    , "EDITOR=vim hkredmine newissue -e ..."
    ]

close       = record Close { issueId = def }
            [ issueId       := def
                            += argPos 0 += typ "ISSUEID"
            ] += help "Close an Issue."
              += groupname "Issues"
              += details
    [ "This command changes and Issue's status to Closed, it's Done Ratio to 100%"
    , "and the Due Date to today, if previously unset:"
    , ""
    , "hkredmine close 154"
    ]

startwork   = record StartWork { issueId = def }
            [ issueId       := def
                            += argPos 0 += typ "ISSUEID"
            ] += help "Start tracking time for an Issue."
              += groupname "Time Tracking"

stopwork    = record StopWork { activityType = def, timeComment = def }
            [ activityType  := Nothing
                            += typ "ACTIVITYNAME"
                            += name "activity"
                            += name "a"
                            += explicit
                            += help "The time entry activity to use."
            , timeComment   := Nothing
                            += typ "STRING"
                            += name "comment"
                            += name "c"
                            += explicit
                            += help "A comment to add with the time entry."
            ] += help "Stop time tracking and submit a time entry."
              += groupname "Time Tracking"
              += details
    [ "This command stops tracking time for the current Issue, prompts for a"
    , "Time Entry Activity and Comment and creates a new Time Entry using"
    , "these values."
    , ""
    , "Flags can be passed to skip the prompts:"
    , ""
    , "hkredmine stopwork"
    , "hkredmine stopwork --comment=\"Responding to User Bug Reports\""
    , "hkredmine stopwork -a Design -c \"Write specs\""
    , ""
    , "Run `hkredmine fields` to get the available Time Entry Activities."
    ]

pause       = record Pause {} []
              += help "Pause time tracking."
              += groupname "Time Tracking"

resume      = record Resume {} []
              += help "Resume time tracking."
              += groupname "Time Tracking"

abort       = record Abort {} []
              += help "Abort time tracking."
              += groupname "Time Tracking"

watch       = record Watch { issueId = def }
            [ issueId := def
                      += argPos 0 += typ "ISSUEID"
            ] += help "Watch an Issue."
              += groupname "Issues"

unwatch     = record Unwatch { issueId = def }
            [ issueId := def
                      += argPos 0 += typ "ISSUEID"
            ] += help "Unwatch an Issue."
              += groupname "Issues"

versions    = record Versions { projectIdent = def }
            [ projectIdent := def
                           += argPos 0 += typ "PROJECTIDENT"
            ] += help "Print all of a Project's Versions."
              += groupname "Versions"

version     = record Version { versionId = def }
            [ versionId := def
                        += argPos 0 += typ "VERSIONID"
            ] += help "Print the details of a Version."
              += groupname "Versions"

nextversion = record NextVersion { projectIdent = def }
            [ projectIdent := def
                           += argPos 0 += typ "PROJECTIDENT"
            ] += help "Print the next Version due for a Project."
              += groupname "Versions"


-- Utils
-- | Transform the arguements of the Issues mode into an IssueFilter.
argsToIssueFilter :: HKRedmine -> Redmine IssueFilter
argsToIssueFilter i@(Issues {}) = do
        filterTracker   <- grabFromName R.getTrackerFromName "Tracker" $ trackerIdent i
        filterPriority  <- grabFromName R.getPriorityFromName "Priority" $ priorityIdent i
        filterStatus    <- R.getStatusFromName $ statusIdent i
        when (isNothing filterStatus && statusIdent i `notElem` ["", "open", "closed", "*"])
           $ redmineLeft "Not a valid Status name."
        return . map packIt $
            [ ("project_id", projectIdent i)
                    | projectIdent i /= "" ] ++
            [ ("tracker_id", show . R.trackerId $ filterTracker)
                    | trackerIdent i /= "" ] ++
            [ ("status_id", show . R.statusId $ fromJust filterStatus)
                    | statusIdent i `notElem` ["", "open", "closed", "*"] ] ++
            [ ("status_id", statusIdent i)
                    | statusIdent i `elem` ["open", "closed", "*"] ] ++
            [ ("priority_id", show . R.priorityId $ filterPriority)
                    | priorityIdent i /= "" ] ++
            [ ("assigned_to_id", assignedTo i)
                    | assignedTo i /= "" ] ++
            [ ("sort", sortByField i)
            , ("limit", show $ limitTo i)
            , ("offset", show $ issueOffset i) ]
        where packIt (s1, s2)   = (s1, BC.pack s2)
argsToIssueFilter _ = redmineLeft "Tried applying an issue filter to non-issue command."

-- | Transform the arguments of the NewIssue mode into a JSON object for
-- the 'createIssue' API function.
argsToIssueObject :: HKRedmine -> Redmine LB.ByteString
argsToIssueObject i@(NewIssue {})   = do
        when (projectIdent i == "" || subject i == "")
           $ redmineLeft "Both a Project Identifier and a Subject are required."
        validProject    <- R.getProjectFromIdent $ projectIdent i
        validTracker    <- grabFromName R.getTrackerFromName "Tracker" $ trackerIdent i
        validPriority   <- grabFromName R.getPriorityFromName "Priority" $ priorityIdent i
        validStatus     <- grabFromName R.getStatusFromName "Status" $ statusIdent i
        currentUser     <- R.getCurrentUser
        actualDescript  <- if editDescript i then liftIO openEditorAndGetContents
                           else return $ description i
        return . encode $ object [ "issue" .= object (
                [ "project_id" .= show (R.projectId validProject)
                , "subject" .= subject i ] ++
                [ "tracker_id" .= show (R.trackerId validTracker)
                        | trackerIdent i /= "" ] ++
                [ "priority_id" .= show (R.priorityId validPriority)
                        | priorityIdent i /= "" ] ++
                [ "status_id" .= show (R.statusId validStatus)
                        | statusIdent i /= "" ] ++
                [ "assigned_to_id" .= show (R.userId currentUser)
                        | not (isNotMine i) ] ++
                [ "fixed_version_id" .= versionId i
                        | versionId i /= 0 ] ++
                [ "description" .= actualDescript
                        | actualDescript /= "" ]
                )
            ]
argsToIssueObject _ = redmineLeft "The wrong command tried parsing newissues arguments."

-- | Get an item from it's name or return an error.
grabFromName :: FromJSON a => (String -> Redmine (Maybe a)) -> String -> String -> Redmine a
grabFromName grab item itemName     = do
        mayValue        <- grab itemName
        when (isNothing mayValue && itemName /= "") $
             redmineLeft $ "Not a valid " ++ item ++ " name."
        return $ fromJust mayValue

-- | Open a temporary file with the user's editor and return it's final
-- contents.
openEditorAndGetContents :: IO String
openEditorAndGetContents         = do
        editor  <- getEnv "EDITOR"
        withSystemTempDirectory "hkredmine" $ \dir -> do
            fn  <- withTempFile dir "hkr.redmine" (\fn' fh -> hClose fh >> return fn')
            writeFile fn "\n"
            _           <- callProcess editor [fn]
            contents    <- readFile fn
            removeFile fn
            return contents
