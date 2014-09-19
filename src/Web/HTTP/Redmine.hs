{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-|
Module          : Web.HTTP.Redmine
Description     : An API Library for the Redmine Bug Tracker
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX

Redmine is a library for interacting with the API for Redmine, a bug
tracker written in Ruby.

Actions are run in the 'Redmine' Monad, which should be supplied with
a 'RedmineConfig'. You will need to override the default 'redAPI' and
'redURL' Config fields.

The following actions are currently supported:

* Fetching All Projects
* Fetching All/Personal Issues


-}
module Web.HTTP.Redmine
        (
        -- * Redmine Monad
          Redmine
        , runRedmine
        , defaultRedmineConfig
        -- * Redmine Types
        , ProjectId
        , Project(..)
        , Projects(..)
        , IssueId
        , Issue(..)
        , Issues(..)
        , Status(..)
        -- ** API-related Types
        , RedmineConfig(..)
        -- * Redmine API Functions
        , getProjects
        , getAllIssues
        , getMyIssues
        , getIssue
        , getStatusFromName
        , getStatusFromId
        , updateIssue
        -- * Formatting
        , projectsTable
        , projectDetail
        , issuesTable
        ) where

import qualified Data.ByteString.Char8 as BC    (pack)
import qualified Data.List as L                 (find)

import Web.HTTP.Redmine.Client
import Web.HTTP.Redmine.Format
import Web.HTTP.Redmine.Types


-- Projects
-- | Retrieve All the 'Projects'
getProjects :: Redmine Projects
getProjects             = getEndPoint GetProjects []


-- Issues
-- | Retrieve All 'Issues' of a Project
getAllIssues :: ProjectId -> Redmine Issues
getAllIssues projectID  = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        ]

-- | Retrieve 'Issues' of a Project Assigned to the User
getMyIssues :: ProjectId -> Redmine Issues
getMyIssues projectID   = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        , ("assigned_to_id", "me")
        , ("offset", "0")
        , ("limit", "100")
        ]

-- | Retrieve an 'Issue'
getIssue :: IssueId -> Redmine Issue
getIssue issueID            = getEndPoint (GetIssue issueID) []

-- | Update the Due Date of an Issue
updateIssue ::  IssueId -> String -> Redmine ()
updateIssue issueID         = putEndPoint $ UpdateIssue issueID

-- Statuses
-- | Retrieve All Available Statuses
getStatuses :: Redmine Statuses
getStatuses = getEndPoint GetStatuses []

-- | Retrieve the 'Status' with the given name.
getStatusFromName :: String              -- ^ The Name of the Status
                  -> Redmine (Maybe Status)
getStatusFromName name      = do
        (Statuses ss) <- getStatuses
        return $ L.find ((== name) . statusName) ss

getStatusFromId :: Integer -> Redmine (Maybe Status)
getStatusFromId i       = do
        (Statuses ss) <- getStatuses
        return $ L.find ((== i) . statusId) ss
