{-# LANGUAGE OverloadedStrings #-}
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
* Fetching All Issue Statuses
* Fetching a Specific Issue Status by Name or Id
* Updating an Issue [TODO: more heavylifting in the library(less user code)]
* Add/Remove Watchers from Images

This is not intended to be a complete API library, just what is required
for `hkredmine`. However, it _could_ be a complete API library,
contributions are welcome...

-}
module Web.HTTP.Redmine
        (
        -- * Redmine Monad
          Redmine
        , runRedmine
        , RedmineConfig(..)
        , defaultRedmineConfig
        , redmineLeft
        , redmineDecode
        , redmineMVar
        , redmineTakeMVar
        -- * Redmine Types
        , IssueFilter
        -- ** ID Types
        , ProjectId
        , ProjectIdent
        , IssueId
        , VersionId
        -- ** Redmine Objects
        , Project(..)
        , Projects(..)
        , Issue(..)
        , Issues(..)
        , Status(..)
        , Activity(..)
        , Tracker(..)
        , Priority(..)
        , Category(..)
        , User(..)
        , Version(..)
        -- * Redmine API Functions
        -- ** Projects
        , getProjects
        , getProjectFromIdent
        -- ** Issues
        , getIssues
        , getVersionsIssues
        , getIssue
        , updateIssue
        , createIssue
        -- ** Issue Statuses
        , getStatuses
        , getStatusFromName
        , getStatusFromId
        -- ** Issue Categories
        , getCategories
        , getCategoryFromName
        , createCategory
        -- ** Trackers
        , getTrackers
        , getTrackerFromName
        -- ** Versions
        , getVersions
        , getVersion
        , getNextVersionDue
        -- ** Time Entries
        , getActivities
        , getActivityFromName
        , addTimeEntry
        -- ** Priorities
        , getPriorities
        , getPriorityFromName
        -- ** Misc
        , getCurrentUser
        , addWatcher
        , removeWatcher
        -- * Formatting
        , projectDetail
        , issueDetail
        , versionDetail
        , projectsTable
        , issuesTable
        , versionTable
        ) where

import qualified Data.ByteString.Char8 as BC    (pack)
import qualified Data.ByteString.Lazy as LB     (ByteString)
import qualified Data.List as L                 (find, sortBy)

import Control.Monad                ((>=>), when, void)
import Data.Aeson                   (FromJSON, object, (.=), encode)
import Data.Function                (on)
import Data.Maybe                   (isJust, isNothing, fromJust)
import Data.Time.Clock              (DiffTime)
import Safe                         (headMay)

import Web.HTTP.Redmine.Client
import Web.HTTP.Redmine.Format
import Web.HTTP.Redmine.Monad
import Web.HTTP.Redmine.Types


-- Projects
-- | Retrieve all 'Projects'.
getProjects :: Redmine [Project]
getProjects                 = do Projects ps <- getEndPoint GetProjects []
                                 return ps

-- | Attempt to retrieve a 'Project'  from a given 'ProjedtIdent'. String
-- versions of a 'ProjectId' are also accepted.
getProjectFromIdent :: ProjectIdent -> Redmine Project
getProjectFromIdent pIdent = do
        maybeProject <- getItemFromField getProjects (\p ->
                                pIdent `elem` [ projectIdentifier p
                                              , show $ projectId p ])
        when (isNothing maybeProject) $ redmineLeft "Not a valid Project Identifier."
        return $ fromJust maybeProject


-- Issues
-- | Retrieve a set of Issues based on an 'IssueFilter'
getIssues :: IssueFilter -> Redmine [Issue]
getIssues f                  = do
        Issues is <- getEndPoint GetIssues f
        return is

-- | Retrieve all 'Issues' of a 'Project'
getProjectsIssues :: ProjectId -> IssueFilter -> Redmine [Issue]
getProjectsIssues pID f     = do
        Issues is <- getEndPoint (GetProjectsIssues pID) f
        return is

-- | Retrieve an 'Issue'.
getIssue :: IssueId -> Redmine Issue
getIssue issueID            = getEndPoint (GetIssue issueID) []

-- | Update an 'Issue'. Currently, you must manually create the request's
-- JSON object.
updateIssue ::  IssueId -> LB.ByteString -> Redmine ()
updateIssue issueID         = putEndPoint $ UpdateIssue issueID

-- | Create an 'Issue'. Make and encode the JSON object yourself.
createIssue :: LB.ByteString -> Redmine Issue
createIssue                 = postEndPoint GetIssues >=> redmineDecode



-- Statuses
-- | Retrieve all available statuses.
getStatuses :: Redmine [Status]
getStatuses                 = do (Statuses ss) <- getEndPoint GetStatuses []
                                 return ss

-- | Retrieve the 'Status' with the given name.
getStatusFromName :: String -> Redmine (Maybe Status)
getStatusFromName name      = getStatusFromField ((== name) . statusName)

-- | Retrieve the 'Status' with the given id.
getStatusFromId :: Integer -> Redmine (Maybe Status)
getStatusFromId i           = getStatusFromField ((== i) . statusId)

-- | Retrieve the 'Status' using the given predicate.
getStatusFromField :: (Status -> Bool) -> Redmine (Maybe Status)
getStatusFromField          = getItemFromField getStatuses

-- | Search a 'Redmine' type using the given predicate.
getItemFromField :: FromJSON a => Redmine [a] -> (a -> Bool) -> Redmine (Maybe a)
getItemFromField items p    = fmap (L.find p) items


-- Time Entries
-- | Retrieve a list of all Time Entry Activities.
getActivities :: Redmine [Activity]
getActivities               = do (Activities as) <- getEndPoint GetActivites []
                                 return as

-- | Retrieve a 'Activity' given a 'activityName'
getActivityFromName :: String -> Redmine (Maybe Activity)
getActivityFromName name = getItemFromField getActivities ((== name) . activityName)

-- | Submit a new Time Entry.
addTimeEntry :: IssueId -> DiffTime -> Activity -> String -> Redmine ()
addTimeEntry i dt a comment = void $ postEndPoint GetTimeEntries postData
        where hours         = fromIntegral (round dt :: Integer) / 3600.0
              postData      = encode $ object
                [ "time_entry" .= object [ "issue_id" .= i
                                         , "hours" .= (hours :: Double)
                                         , "activity_id" .= activityId a
                                         , "comments" .= comment
                                         ] ]

-- Trackers
-- | Retrieve a list of all Trackers.
getTrackers :: Redmine [Tracker]
getTrackers                 = do Trackers ts    <- getEndPoint GetTrackers []
                                 return ts

-- | Retrieve a 'Tracker' given it's 'trackerName'.
getTrackerFromName :: String -> Redmine (Maybe Tracker)
getTrackerFromName name     = getItemFromField getTrackers ((== name) . trackerName)


-- Priorities
-- | Retrieve a list of all Priorities.
getPriorities :: Redmine [Priority]
getPriorities               = do Priorities ps  <- getEndPoint GetPriorities []
                                 return ps

-- | Retrieve a 'Priority' given it's 'priorityName'.
getPriorityFromName :: String -> Redmine (Maybe Priority)
getPriorityFromName name    = getItemFromField getPriorities ((== name) . priorityName)


-- Categories
-- | Retrieve a list of every 'Category' in a 'Project'.
getCategories :: ProjectId -> Redmine [Category]
getCategories p             = do Categories cs  <- getEndPoint (GetCategories  p) []
                                 return cs

-- | Retrieve a 'Category' from a 'ProjectId' and a 'categoryName'.
getCategoryFromName :: ProjectId -> String -> Redmine (Maybe Category)
getCategoryFromName p name  = getItemFromField (getCategories p) ((== name) . categoryName)

-- | Create a category from a JSON object.
createCategory :: ProjectId -> LB.ByteString -> Redmine Category
createCategory pId = postEndPoint (GetCategories pId) >=> redmineDecode


-- Users
-- | Retrieve the current 'User'.
getCurrentUser :: Redmine User
getCurrentUser              = getEndPoint GetCurrentUser []


-- Watching
-- | Add a watcher to an 'Issue'.
addWatcher :: IssueId -> User -> Redmine ()
addWatcher i user           = void $ postEndPoint (AddWatcher i) postData
        where postData      = encode $ object [ "user_id" .= userId user ]

-- | Remove a watcher from an 'Issue'.
removeWatcher :: IssueId -> User -> Redmine ()
removeWatcher i user        = deleteEndPoint $ RemoveWatcher i $ userId user


-- Versions
-- | Retrieve all 'Versions' of a 'Project' from the 'projectId'.
getVersions :: ProjectId -> Redmine [Version]
getVersions p               = do Versions vs <- getEndPoint (GetVersions p) []
                                 return vs

-- | Retrieve all 'Versions' from all 'Projects'.
getAllVersions :: Redmine [Version]
getAllVersions              = do Projects ps <- getEndPoint GetProjects []
                                 let projectIds = map projectId ps
                                 vs <- mapM getVersions projectIds
                                 return $ concat vs

-- | Retrieve a 'Version' from it's id.
getVersion :: VersionId -> Redmine Version
getVersion v                = do versions <- getAllVersions
                                 let versionIds = map versionId versions
                                 if v `elem` versionIds
                                 then getEndPoint (GetVersion v) []
                                 else redmineLeft "Version does not exist."

-- | Retrieve all 'Issues' of a 'Version'.
getVersionsIssues :: Version -> IssueFilter -> Redmine [Issue]
getVersionsIssues v f       = getProjectsIssues (versionProjectId v) $ f ++
        [ ("fixed_version_id", BC.pack . show $ versionId v) ]

-- | Retrieve the next open 'Version' of a 'Project' with the soonest
-- 'versionDueDate'.
getNextVersionDue :: ProjectId -> Redmine (Maybe Version)
getNextVersionDue p         = do
        vs  <- getVersions p
        return . headMay . L.sortBy (compare `on` versionDueDate)
               . filter ((== "open") . versionStatus)
               . filter (isJust . versionDueDate) $ vs
