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

import System.Console.CmdArgs
import Control.Monad.IO.Class   (liftIO)

import Web.HTTP.Redmine         (Redmine)

import Main.Actions

-- | Usage modes options for the CLI interface.
data HKRedmine
        = Use           { accountName       :: String }
        | Status        { }
        | Projects      { }
        | Project       { projectIdent      :: String }
        | Issues        { projectIdent      :: String }
        | StartWork     { issueId           :: Integer }
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
        Projects        -> printProjects
        Project p       -> printProject p
        Issues p        -> printProjectsIssues p
        StartWork i     -> startTimeTracking i
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
        , startwork , pause, resume, abort
        , watch, unwatch
        , versions, version, nextversion ]
        += help "A Redmine CLI client"
        += program "hkredmine"
        += summary "HKRedmine v0.1"


-- | Default options for modes
use, status, projects, project, issues, startwork, pause, resume,
     abort, watch, unwatch, versions, version, nextversion :: Annotate Ann
use = record Use { accountName = def }
    [ accountName := def
                  += argPos 0
                  += typ "ACCOUNT"
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
            [ projectIdent := def
                           += argPos 0 += typ "PROJECT"
            ] += help "Print the details of a Project."

issues      = record Issues { projectIdent = def }
            [ projectIdent := def
                           += argPos 0 += typ "PROJECT"
            ] += help "Print all Issues of a Project."
              += groupname "Issues"

startwork   = record StartWork { issueId = def }
            [ issueId := def
                      += argPos 0 += typ "ISSUE"
            ] += help "Start tracking time for an Issue."
              += groupname "Time Tracking"

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
                      += argPos 0 += typ "ISSUE"
            ] += help "Watch an Issue."
              += groupname "Issues"

unwatch     = record Unwatch { issueId = def }
            [ issueId := def
                      += argPos 0 += typ "ISSUE"
            ] += help "Unwatch an Issue."
              += groupname "Issues"

versions    = record Versions { projectIdent = def }
            [ projectIdent := def
                           += argPos 0 += typ "PROJECT"
            ] += help "Print all of a Project's Versions."
              += groupname "Versions"

version     = record Version { versionId = def }
            [ versionId := def
                        += argPos 0 += typ "VERSION"
            ] += help "Print the details of a Version."
              += groupname "Versions"

nextversion = record NextVersion { projectIdent = def }
            [ projectIdent := def
                           += argPos 0 += typ "PROJECT"
            ] += help "Print the next Version due for a Project."
              += groupname "Versions"
