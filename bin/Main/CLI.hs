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


import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Data.Maybe               (isNothing, isJust, fromJust)
import System.Console.CmdArgs

import Web.HTTP.Redmine         (Redmine, IssueFilter, redmineLeft)

import Main.Actions

-- | Usage modes options for the CLI interface.
data HKRedmine
        = Use           { accountName       :: String }
        | Status        { }
        | Projects      { }
        | Project       { projectIdent      :: String }
        | Issues        { projectIdent      :: String
                        , trackerIdent      :: String
                        , statusIdent       :: String
                        , assignedTo        :: String
                        , sortByField       :: String
                        , limitTo           :: Integer
                        , issueOffset       :: Integer }
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



argsToIssueFilter :: HKRedmine -> Redmine IssueFilter
argsToIssueFilter i@(Issues {}) = do
        mayTracker <- R.getTrackerFromName $ trackerIdent i
        when (isNothing mayTracker && trackerIdent i /= "") $
             redmineLeft "Not a valid Tracker name."
        return . map packIt $
            [ ("project_id", projectIdent i)
                    | projectIdent i /= "" ] ++
            [ ("tracker_id", show . R.trackerId . fromJust $ mayTracker)
                    | isJust mayTracker ] ++
            [ ("status_id", statusIdent i)
            , ("assigned_to_id", assignedTo i)
            , ("sort", sortByField i)
            , ("limit", show $ limitTo i)
            , ("offset", show $ issueOffset i) ]
        where packIt (s1, s2) = (s1, BC.pack s2)
argsToIssueFilter _ = error "Tried applying an issue filter to non-issue command."


-- | Route a 'HKRedmine' Mode to a 'Redmine' Action.
dispatch :: HKRedmine -> Redmine ()
dispatch m = case m of
        Use a           -> liftIO $ switchAccount a
        Status          -> liftIO printStatus
        Projects        -> printProjects
        Project p       -> printProject p
        i@(Issues {})   -> argsToIssueFilter i >>= printIssues
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
        [ use, status
        , projects, project
        , issues
        , startwork, stopwork, pause, resume, abort
        , watch, unwatch
        , versions, version, nextversion ]
        += help "A Redmine CLI client"
        += program "hkredmine"
        += summary "HKRedmine v0.1"


-- | Default options for modes
use, status, projects, project, issues, startwork, stopwork, pause, resume,
     abort, watch, unwatch, versions, version, nextversion :: Annotate Ann
use = record Use { accountName = def }
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

projects    = record Projects {} []
              += help "Print all Projects."

project     = record Project { projectIdent = def }
            [ projectIdent  := def
                            += argPos 0 += typ "PROJECTIDENT"
            ] += help "Print the details of a Project."

issues      = record Issues { projectIdent = def, statusIdent = def
                            , trackerIdent = def, assignedTo = def
                            , sortByField = def, limitTo = def
                            , issueOffset = def }
            [ projectIdent  := def
                            += typ "PROJECTIDENT"
                            += name "p"
                            += name "project"
                            += groupname "filter"
                            += explicit
                            += help "A Project's Identifier or ID"
            , statusIdent   := "open"
                            += typ "STATUS"
                            += name "status"
                            += name "s"
                            += groupname "filter"
                            += explicit
                            += help "A Status Name"
            , trackerIdent  := def
                            += typ "TRACKERNAME"
                            += name "tracker"
                            += name "t"
                            += groupname "filter"
                            += explicit
                            += help "A Tracker Name"
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
                                     "Valid options are \"project\", \"priority\", " ++
                                     "\"status\", and \"category\"" )
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
              += details [ "Example Usage:"
                         , "hkredmine issues -u"
                         , "hkredmine issues -n 50 -p accounting-app -S priority"
                         ]

startwork   = record StartWork { issueId = def }
            [ issueId := def
                      += argPos 0 += typ "ISSUEID"
            ] += help "Start tracking time for an Issue."
              += groupname "Time Tracking"

stopwork    = record StopWork { activityType = def, timeComment = def }
            [ activityType := Nothing
                           += typ "ACTIVITYNAME"
                           += name "activity"
                           += name "a"
                           += explicit
                           += help "The time entry activity to use."
            , timeComment  := Nothing
                           += typ "COMMENT"
                           += name "comment"
                           += name "c"
                           += explicit
                           += help "A comment to add with the time entry."
            ] += help "Stop time tracking and submit a time entry."
              += groupname "Time Tracking"
              += details [ "This command stops tracking time for the current Issue, prompts"
                         , "for a Time Entry Activity and Comment and creates a new Time"
                         , "Entry using these values."
                         , ""
                         , "Flags can be passed to skip the prompts:"
                         , ""
                         , "hkredmine stopwork"
                         , "hkredmine stopwork --comment=\"Responding to User Bug Reports\""
                         , "hkredmine stopwork -a Design -c \"Write specs\""
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
