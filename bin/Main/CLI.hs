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


import Control.Applicative      ((<$>))
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
        | Categories    { projectIdent      :: String }
        | Projects      { }
        | Project       { projectIdent      :: String }
        | Issue         { issueId           :: Integer }
        | Issues        { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
        | Watched       { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
        | NewIssue      { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , subject           :: String
                        , description       :: String
                        , versionId         :: Integer
                        , doneRatio         :: Integer
                        , editDescript      :: Bool
                        , isNotMine         :: Bool }
        | Update        { issueId           :: Integer
                        , projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , subject           :: String
                        , description       :: String
                        , versionId         :: Integer
                        , doneRatio         :: Integer
                        , editDescript      :: Bool
                        , notes             :: String }
        | Close         { issueId           :: Integer
                        , comment           :: Maybe String }
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
        Categories p    -> printCategories p
        Projects        -> printProjects
        Project p       -> printProject p
        Issue i         -> printIssue i
        i@(Issues {})   -> argsToIssueFilter i >>= printIssues
        i@(Watched {})  -> argsToIssueFilter i >>=
                           printIssues . (++ [ ("watcher_id", "me" ) ])
        i@(NewIssue {}) -> argsToIssueObject i >>= createNewIssue
        i@(Update {})   -> argsToIssueObject i >>= R.updateIssue (issueId i) >>
                           liftIO (putStrLn $ "Updated Issue #" ++ show (issueId i) ++ ".")
        Close i s       -> closeIssue i s
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
        [ use, status, fields, categories
        , project, projects
        , issue, issues, watched, newissue, update, close
        , startwork, stopwork, pause, resume, abort
        , watch, unwatch
        , version, versions, nextversion ]
        += help "A Redmine CLI client"
        += program "hkredmine"
        += summary "HKRedmine v0.1"


-- | Default options for modes
use, status, fields, categories, projects, project, issue, issues, watched,
    newissue, update, close, startwork, stopwork, pause, resume, abort,
    watch, unwatch, versions, version, nextversion :: Annotate Ann
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
              += help "Print available field values(Statuses, Priorities, etc.)."

categories  = record Categories { projectIdent = def }
            [ projectIdent  := def
                            += argPos 0 += typ "PROJECTIDENT"
            ] += help "Print a Project's Categories."


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
                            , categoryIdent = def, assignedTo = def
                            , sortByField = def, limitTo = def
                            , issueOffset = def }
            [ projectIdent  := def
                            += typ "PROJECTIDENT"
                            += name "project"
                            += name "p"
                            += groupname "Filter"
                            += explicit
                            += help "A Project's Identifier or ID"
            , statusIdent   := "open"
                            += typ "STATUS"
                            += name "status"
                            += name "s"
                            += groupname "Filter"
                            += explicit
                            += help "A Status Name. open and closed are also valid choices"
            , trackerIdent  := def
                            += typ "TRACKERNAME"
                            += name "tracker"
                            += name "t"
                            += groupname "Filter"
                            += explicit
                            += help "A Tracker Name"
            , priorityIdent := def
                            += typ "PRIORITYNAME"
                            += name "priority"
                            += name "i"
                            += groupname "Filter"
                            += explicit
                            += help "A Priority Name"
            , categoryIdent := def
                            += typ "CATEGORYNAME"
                            += name "category"
                            += name "c"
                            += groupname "Filter"
                            += explicit
                            += help "A Category Name. Requires filtering by Project"
            , assignedTo    := ""
                            += opt ("me" :: String)
                            += name "userid"
                            += name "u"
                            += groupname "Filter"
                            += explicit
                            += help "A User ID(defaults to yours)"
            , issueOffset   := 0
                            += typ "INT"
                            += name "offset"
                            += name "o"
                            += explicit
                            += groupname "Range"
                            += help "Start listing Issues at this row number"
            , sortByField   := "updated_on"
                            += typ "FIELD"
                            += name "sort"
                            += name "S"
                            += explicit
                            += groupname "Sort"
                            += help ("Comma-separated columns to sort by. Append " ++
                                     "':desc' to reverse the sorting. Valid options " ++
                                     "are project, priority, status, category, " ++
                                     "updated_on and created_on" )
            , limitTo       := 20
                            += typ "INT"
                            += name "limit"
                            += name "n"
                            += name "l"
                            += explicit
                            += groupname "Range"
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
    , "hkredmine issues -S priority:desc"
    ]

watched     = record Watched { projectIdent = def, statusIdent = def
                             , trackerIdent = def, priorityIdent = def
                             , categoryIdent = def, assignedTo = def
                             , sortByField = def, limitTo = def
                             , issueOffset = def } []
              += help "Filter and Print your Watched Issues."
              += groupname "Issues"

newissue    = record NewIssue { projectIdent = def, trackerIdent = def
                              , statusIdent = def, priorityIdent = def
                              , categoryIdent = def, isNotMine = def
                              , subject = def, description = def
                              , doneRatio = def, versionId = def
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
            , categoryIdent := def
                            += typ "CATEGORYNAME"
                            += name "category"
                            += name "c"
                            += groupname "Optional"
                            += explicit
                            += help "A Category Name"
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
            , doneRatio     := def
                            += name "done-ratio"
                            += name "r"
                            += explicit
                            += groupname "Optional"
                            += help "The Percentage Completed"
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

update      = record Update { issueId = def, projectIdent = def
                            , trackerIdent = def, statusIdent = def
                            , priorityIdent = def, categoryIdent = def
                            , subject = def, description = def
                            , versionId = def, doneRatio = def
                            , editDescript = False , notes = def }
            [ issueId       := def
                            += argPos 0 += typ "ISSUEID"
            , projectIdent  := def
                            += typ "PROJECTIDENT"
                            += name "project"
                            += name "p"
                            += groupname "Optional"
                            += explicit
                            += help "A Project's Identifier"
            , subject       := def
                            += typ "STRING"
                            += name "subject"
                            += name "s"
                            += groupname "Optional"
                            += explicit
                            += help "The Issue's Subject"
            , notes         := def
                            += typ "STRING"
                            += name "comment"
                            += name "n"
                            += groupname "Optional"
                            += explicit
                            += help "A Comment about the Update"
            ] += help "Update an New Issue."
              += groupname "Issues"


close       = record Close { issueId = def, comment = def }
            [ issueId       := def
                            += argPos 0 += typ "ISSUEID"
            , comment       := def
                            += typ "STRING"
                            += name "comment"
                            += name "c"
                            += explicit
                            += help "A Comment to Include"
            ] += help "Close an Issue."
              += groupname "Issues"
              += details
    [ "This command changes and Issue's status to Closed, it's Done Ratio to 100%"
    , "and the Due Date to today, if previously unset:"
    , ""
    , "hkredmine close 154"
    , ""
    , "You can modify the comment that is added:"
    , ""
    , "hkredmine close 154 -c \"Merged into master\""
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
        filterCategory  <- case (categoryIdent i, projectIdent i) of
                ("", "")    -> return ""
                ("", _)     -> return ""
                (_, "")     -> redmineLeft $ "You must filter by Project if " ++
                                              "you want to filter by Category."
                (c, p)      -> do proj <- R.getProjectFromIdent p
                                  show . R.categoryId <$>
                                         grabFromName (R.getCategoryFromName $ R.projectId proj)
                                                      "Category" c
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
            [ ("category_id", filterCategory)
                    | filterCategory /= "" ] ++
            [ ("sort", sortByField i)
            , ("limit", show $ limitTo i)
            , ("offset", show $ issueOffset i) ]
        where packIt (s1, s2)   = (s1, BC.pack s2)
argsToIssueFilter w@(Watched {})    = argsToIssueFilter Issues
            { projectIdent          = projectIdent w
            , trackerIdent          = trackerIdent w
            , statusIdent           = statusIdent w
            , priorityIdent         = priorityIdent w
            , categoryIdent         = categoryIdent w
            , assignedTo            = assignedTo w
            , sortByField           = sortByField w
            , limitTo               = limitTo w
            , issueOffset           = issueOffset w }
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
        validCategory   <- grabFromName (R.getCategoryFromName $ R.projectId validProject)
                                         "Category" $ categoryIdent i
        currentUser     <- R.getCurrentUser
        actualDescript  <- if editDescript i then liftIO getTextFromEditor
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
                [ "category_id" .= show (R.categoryId validCategory)
                        | categoryIdent i  /= "" ] ++
                [ "done_ratio" .= show (doneRatio i)
                        | doneRatio i /= 0 ]  ++
                [ "description" .= actualDescript
                        | actualDescript /= "" ]
                )
            ]
argsToIssueObject u@(Update {})     = do
        i               <- R.getIssue $ issueId u
        validProject    <- if projectIdent u /= ""
                           then R.getProjectFromIdent $ projectIdent u
                           else R.getProjectFromIdent . show $ R.issueProjectId i
        validTracker    <- grabFromName R.getTrackerFromName "Tracker" $ trackerIdent u
        validPriority   <- grabFromName R.getPriorityFromName "Priority" $ priorityIdent u
        validStatus     <- grabFromName R.getStatusFromName "Status" $ statusIdent u
        validCategory   <- grabFromName (R.getCategoryFromName $ R.projectId validProject)
                                         "Category" $ categoryIdent u
        actualDescript  <- if editDescript u
                           then liftIO $ editTextInEditor $ R.issueDescription i
                           else return $ description u
        return . encode $ object [ "issue" .= object (
                [ "project_id" .= show (R.projectId validProject) ] ++
                [ "subject" .= subject u
                        | subject u /= "" ] ++
                [ "tracker_id" .= show (R.trackerId validTracker)
                        | trackerIdent u /= "" ] ++
                [ "priority_id" .= show (R.priorityId validPriority)
                        | priorityIdent u /= "" ] ++
                [ "status_id" .= show (R.statusId validStatus)
                        | statusIdent u /= "" ] ++
                [ "fixed_version_id" .= versionId u
                        | versionId u /= 0 ] ++
                [ "category_id" .= show (R.categoryId validCategory)
                        | categoryIdent u  /= "" ] ++
                [ "done_ratio" .= show (doneRatio u)
                        | doneRatio u /= 0 ]  ++
                [ "description" .= actualDescript
                        | actualDescript /= "" ] ++
                [ "notes" .= notes u
                        | notes u /= "" ]
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
getTextFromEditor :: IO String
getTextFromEditor                   = editTextInEditor "\n"

-- | Write a string to a Temporary File, open the file in the $EDITOR and
-- return the final contents.
editTextInEditor :: String -> IO String
editTextInEditor s              =
        withSystemTempDirectory "hkredmine" $ \dir -> do
            fn  <- withTempFile dir "hkr.redmine" (\fn' fh -> hClose fh >> return fn')
            writeFile fn s
            contents <- openInEditorAndRead fn
            removeFile fn >> return contents

-- | Open a file in the User's $EDITOR, read and return the contents
-- afterwards.
openInEditorAndRead :: FilePath -> IO String
openInEditorAndRead fn              = getEnv "EDITOR" >>= flip callProcess [fn]
                                   >> readFile fn
