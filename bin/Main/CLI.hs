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
import Data.Maybe               (isNothing, isJust, fromJust)
import System.Console.CmdArgs
import System.Directory         (removeFile)
import System.Environment       (getEnv)
import System.IO                (hClose)
import System.IO.Temp           (withSystemTempDirectory, withTempFile)
import System.Process           (callProcess)

import Web.HTTP.Redmine         (Redmine, IssueFilter, redmineLeft,
                                 redmineMVar, redmineTakeMVar)

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
        | NewCat        { projectIdent      :: String
                        , categoryName      :: String
                        , assignToMe        :: Bool }
        | StartWork     { issueId           :: Integer }
        | StopWork      { activityType      :: Maybe String
                        , timeComment       :: Maybe String }
        | Pause         { }
        | Resume        { }
        | Abort         { }
        | Watch         { issueId           :: Integer }
        | Unwatch       { issueId           :: Integer }
        | Versions      { projectIdent      :: String }
        | Version       { versionId         :: Integer
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
        | NextVersion   { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , priorityIdent     :: String
                        , categoryIdent     :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
         deriving (Show, Data, Typeable)


-- | Route a 'HKRedmine' Mode to a 'Redmine' Action.
dispatch :: HKRedmine -> Redmine ()
dispatch m = case m of
    Use a               -> liftIO $ switchAccount a
    Status              -> liftIO printStatus
    Fields              -> printFields
    Categories p        -> printCategories p
    Projects            -> printProjects
    Project p           -> printProject p
    Issue i             -> printIssue i
    i@(Issues {})       -> argsToIssueFilter i >>= printIssues
    i@(Watched {})      -> argsToIssueFilter i >>=
                            printIssues . (++ [ ("watcher_id", "me" ) ])
    i@(NewIssue {})     -> argsToIssueObject i >>= createNewIssue
    i@(Update {})       -> argsToIssueObject i >>= R.updateIssue (issueId i) >>
                            liftIO (putStrLn $ "Updated Issue #" ++
                                                show (issueId i) ++ ".")
    Close i s           -> closeIssue i s
    NewCat p c me       -> newCategory p c me
    StartWork i         -> startTimeTracking i
    StopWork a c        -> stopTimeTracking a c
    Pause               -> liftIO pauseTimeTracking
    Resume              -> liftIO resumeTimeTracking
    Abort               -> liftIO abortTimeTracking
    Watch i             -> watchIssue i
    Unwatch i           -> unwatchIssue i
    Versions p          -> printVersions p
    v@(Version {})      -> argsToIssueFilter v >>= printVersion (versionId v)
    nv@(NextVersion {}) -> argsToIssueFilter nv >>=
                           printNextVersion (projectIdent nv)


-- | Available usage modes
hkredmine :: Annotate Ann
hkredmine = modes_
        [ use, status
        , project, projects
        , issue, issues, watched, newissue, update, close, watch, unwatch
        , fields, categories, newcategory
        , startwork, stopwork, pause, resume, abort
        , version, versions, nextversion ]
        += help "A Redmine CLI client"
        += program "hkredmine"
        += summary "HKRedmine v0.1"


-- | Default options for modes
use, status, fields, categories, newcategory, projects, project, issue,
    issues, watched, newissue, update, close, startwork, stopwork, pause,
    resume, abort, watch, unwatch, versions, version,
    nextversion :: Annotate Ann
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
              += groupname "Options"

categories  = record Categories { projectIdent = def } [ projectArg ]
              += help "Print a Project's Categories."
              += groupname "Options"


projects    = record Projects {} []
              += help "Print all Projects."
              += groupname "Projects"

project     = record Project { projectIdent = def } [ projectArg ]
              += help "Print the details of a Project."
              += groupname "Projects"


issue       = record Issue { issueId = def } [ issueArg ]
              += help "Print the details of an Issue."
              += groupname "Issues"

issues      = record Issues { projectIdent = def, statusIdent = def
                            , trackerIdent = def, priorityIdent = def
                            , categoryIdent = def, assignedTo = def
                            , sortByField = def, limitTo = def
                            , issueOffset = def }
            [ projectFlag   += groupname "Filter"
            , statusFlag "open" += groupname "Filter" += name "s"
            , trackerFlag   += groupname "Filter"
            , priorityFlag  += groupname "Filter"
            , categoryFlag  += help "A Category Name. Requires a Project"
                            += groupname "Filter"
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
            [ projectFlag   += groupname "Required"
            , subjectFlag   += groupname "Required"
            , description   := def
                            += typ "STRING"
                            += name "description"
                            += name "d"
                            += explicit
                            += groupname "Optional"
                            += help "A Full Description About the Issue"
            , statusFlag def += groupname "Optional" += name "a"
            , trackerFlag   += groupname "Optional"
            , priorityFlag  += groupname "Optional"
            , categoryFlag  += help "A Category Name"
                            += groupname "Optional"
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
            [ issueArg
            , projectFlag   += groupname "Optional"
            , subjectFlag   += groupname "Optional"
            , notes         := def
                            += typ "STRING"
                            += name "comment"
                            += name "n"
                            += groupname "Optional"
                            += explicit
                            += help "A Comment about the Update"
            ] += help "Update an New Issue."
              += groupname "Issues"

newcategory = record NewCat { projectIdent = def, categoryName = def
                            , assignToMe = def }
            [ projectArg
            , categoryName  := def += argPos 1 += typ "NAME"
            , assignToMe    := def
                            += name "me"
                            += name "m"
                            += explicit
                            += help "Assign Issue's to You"
            ] += name "newcategory"
              += help "Create a Category."
              += groupname "Options"
              += details [ "hkredmine newcategory my-proj \"My Category\" -m" ]


close       = record Close { issueId = def, comment = def }
            [ issueArg
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

watch       = record Watch { issueId = def } [ issueArg ]
              += help "Watch an Issue."
              += groupname "Issues"

unwatch     = record Unwatch { issueId = def } [ issueArg ]
              += help "Unwatch an Issue."
              += groupname "Issues"


startwork   = record StartWork { issueId = def } [ issueArg ]
              += help "Start tracking time for an Issue."
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

version     = record Version { versionId = def, statusIdent = def
                             , trackerIdent = def, priorityIdent = def
                             , categoryIdent = def, assignedTo = def
                             , sortByField = def, limitTo = def
                             , issueOffset = def }
            [ versionId := def
                        += argPos 0 += typ "VERSIONID"
            , statusFlag "open" += groupname "Filter" += name "s"
            , trackerFlag   += groupname "Filter"
            , priorityFlag  += groupname "Filter"
            , categoryFlag  += help "A Category Name. Requires a Project"
                            += groupname "Filter"
            ] += help "Print the details of a Version."
              += groupname "Versions"

nextversion = record NextVersion
                             { projectIdent = def, statusIdent = def
                             , trackerIdent = def, priorityIdent = def
                             , categoryIdent = def, assignedTo = def
                             , sortByField = def, limitTo = def
                             , issueOffset = def } [ projectArg ]
              += help "Print the next Version due for a Project."
              += groupname "Versions"

versions    = record Versions { projectIdent = def } [ projectArg ]
              += help "Print all of a Project's Versions."
              += groupname "Versions"


-- Standard Arguments
projectArg, issueArg, projectFlag, trackerFlag, priorityFlag, categoryFlag,
    subjectFlag :: Annotate Ann
-- | Make the projectIdent the first required argument.
projectArg          = projectIdent := def += argPos 0 += typ "PROJECTIDENT"
-- | Make the issueId the first required arugment.
issueArg            = issueId := def += argPos 0 += typ "ISSUEID"
-- | An optional flag for a projectIdent argument.
projectFlag         = projectIdent := def
                    += typ "PROJECTIDENT"
                    += name "project"
                    += name "p"
                    += explicit
                    += help "A Project's Identifier or ID"
-- | An optional flag for a trackerIdent argument.
trackerFlag         =  trackerIdent  := def
                    += typ "TRACKERNAME"
                    += name "tracker"
                    += name "t"
                    += explicit
                    += help "A Tracker Name"
-- | An optional flag for a priorityIdent argument.
priorityFlag        = priorityIdent := def
                    += typ "PRIORITYNAME"
                    += name "priority"
                    += name "i"
                    += explicit
                    += help "A Priority Name"
-- | An optional flag for a categoryIdent argument.
categoryFlag        = categoryIdent := def
                    += typ "CATEGORYNAME"
                    += name "category"
                    += name "c"
                    += explicit
-- | An optional flag for a subject argument.
subjectFlag         = subject := def
                    += typ "STRING"
                    += name "subject"
                    += name "s"
                    += explicit
                    += help "The Issue's Subject"
-- | An optional flag for a statusIdent argument.
statusFlag :: String -> Annotate Ann
statusFlag d        = statusIdent := d
                    += typ "STATUS"
                    += name "status"
                    += explicit
                    += help "A Status Name"


-- Utils
-- | Transform the arguements of the Issues mode into an IssueFilter. The
-- special statuses "open", "closed" and "*" are also allowed.
argsToIssueFilter :: HKRedmine -> Redmine IssueFilter
argsToIssueFilter i@(Issues {}) = do
        projectFork     <- redmineMVar . R.getProjectFromIdent $ projectIdent i
        trackerFork     <- redmineMVar . grabFromName R.getTrackerFromName "Tracker"
                         $ trackerIdent i
        priorityFork    <- redmineMVar . grabFromName R.getPriorityFromName "Priority"
                         $ priorityIdent i
        statusFork      <- redmineMVar . R.getStatusFromName $ statusIdent i
        filterProject   <- if projectIdent i /= "" then Just <$> redmineTakeMVar projectFork
                           else return Nothing
        categoryFork    <- redmineMVar $ case (categoryIdent i, projectIdent i) of
            ("", "")    -> return ""
            ("", _)     -> return ""
            (_, "")     -> redmineLeft $ "You must filter by Project if " ++
                                            "you want to filter by Category."
            (c, _)      -> show . R.categoryId <$>
                           grabFromName (R.getCategoryFromName . R.projectId $
                                         fromJust filterProject) "Category" c
        filterTracker   <- redmineTakeMVar trackerFork
        filterPriority  <- redmineTakeMVar priorityFork
        filterStatus    <- redmineTakeMVar statusFork
        filterCategory  <- redmineTakeMVar categoryFork
        when (isNothing filterStatus && statusIdent i `notElem` ["", "open", "closed", "*"])
           $ redmineLeft "Not a valid Status name."
        return . map packIt $
            [ ("project_id", R.projectIdentifier $ fromJust filterProject)
                    | isJust filterProject ] ++
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
argsToIssueFilter v@(Version {})    = do
            vers                    <- R.getVersion $ versionId v
            argsToIssueFilter Issues {
              projectIdent          = show $ R.versionProjectId vers
            , trackerIdent          = trackerIdent v
            , statusIdent           = statusIdent v
            , priorityIdent         = priorityIdent v
            , categoryIdent         = categoryIdent v
            , assignedTo            = assignedTo v
            , sortByField           = sortByField v
            , limitTo               = limitTo v
            , issueOffset           = issueOffset v }
argsToIssueFilter v@(NextVersion {})    = argsToIssueFilter Issues
            { projectIdent          = projectIdent v
            , trackerIdent          = trackerIdent v
            , statusIdent           = statusIdent v
            , priorityIdent         = priorityIdent v
            , categoryIdent         = categoryIdent v
            , assignedTo            = assignedTo v
            , sortByField           = sortByField v
            , limitTo               = limitTo v
            , issueOffset           = issueOffset v }
argsToIssueFilter _ = redmineLeft "Tried applying an issue filter to non-issue command."

-- | Transform the arguments of the NewIssue mode into a JSON object for
-- the 'createIssue' API function.
argsToIssueObject :: HKRedmine -> Redmine LB.ByteString
argsToIssueObject i@(NewIssue {})   = do
        when (projectIdent i == "" || subject i == "")
           $ redmineLeft "Both a Project Identifier and a Subject are required."
        (validProject,
         validTracker,
         validPriority,
         validStatus,
         validCategory) <- validateIssueOptions (projectIdent i) (trackerIdent i)
                           (priorityIdent i) (statusIdent i) (categoryIdent i)
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
                ) ]
argsToIssueObject u@(Update {})     = do
        i               <- R.getIssue $ issueId u
        (validProject,
         validTracker,
         validPriority,
         validStatus,
         validCategory) <- validateIssueOptions
                           (if projectIdent u /= "" then projectIdent u
                            else show $ R.issueProjectId i)
                           (trackerIdent u) (priorityIdent u) (statusIdent u)
                           (categoryIdent u)
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
                ) ]
argsToIssueObject _ = redmineLeft "The wrong command tried parsing newissues arguments."

-- | Concurrently validate a Project, Tracker, Priority, Status and Category.
validateIssueOptions :: R.ProjectIdent          -- ^ The Project Identifier
                     -> String                  -- ^ The Tracker Name
                     -> String                  -- ^ The Priority Name
                     -> String                  -- ^ The Status Name
                     -> String                  -- ^ The Category Name
                     -> Redmine (R.Project, R.Tracker, R.Priority, R.Status, R.Category)
validateIssueOptions p t i s c      = do
        projectFork     <- redmineMVar . R.getProjectFromIdent $ p
        trackerFork     <- redmineMVar . grabFromName R.getTrackerFromName "Tracker" $ t
        priorityFork    <- redmineMVar . grabFromName R.getPriorityFromName "Priority" $ i
        statusFork      <- redmineMVar . grabFromName R.getStatusFromName "Status" $ s
        validProject    <- redmineTakeMVar projectFork
        categoryFork    <- redmineMVar . grabFromName (R.getCategoryFromName $
                           R.projectId validProject) "Category" $ c
        validTracker    <- redmineTakeMVar trackerFork
        validPriority   <- redmineTakeMVar priorityFork
        validStatus     <- redmineTakeMVar statusFork
        validCategory   <- redmineTakeMVar categoryFork
        return (validProject, validTracker, validPriority, validStatus, validCategory)


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
editTextInEditor s                  =
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
