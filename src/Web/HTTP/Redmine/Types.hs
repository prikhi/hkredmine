{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
-
- This Module contains the Redmine-specific Types and their JSON parsing
- instances.
-
-}
module Web.HTTP.Redmine.Types
        ( IssueFilter
        , EndPoint(..)
        , ProjectId
        , ProjectIdent
        , IssueId
        , VersionId
        , Project(..)
        , Projects(..)
        , Issue(..)
        , Issues(..)
        , Status(..)
        , Statuses(..)
        , User(..)
        , Version(..)
        , Versions(..)
        , Activity(..)
        , Activities(..)
        , Tracker(..)
        , Trackers(..)
        , Priority(..)
        , Priorities(..)
        ) where

import Control.Applicative  ((<$>), (<*>))
import Data.Aeson           ((.:), (.:?), (.=), (.!=), object,
                             FromJSON, parseJSON, Value(Object))

import qualified Data.Aeson.Types as AT   (Parser, Object)
import qualified Data.ByteString as B
import qualified Data.Text as T


-- | An IssueFilter is a list of parameters and values.
type IssueFilter = [(B.ByteString, B.ByteString)]


-- | API EndPoints
data EndPoint    =
          GetProjects
        | GetStatuses
        | GetCurrentUser
        | GetVersion VersionId
        | GetVersions ProjectId
        | GetActivites
        | GetTimeEntries
        | GetTrackers
        | GetPriorities
        | GetIssues
        | GetProjectsIssues ProjectId
        | GetIssue IssueId
        | UpdateIssue IssueId
        | AddWatcher IssueId
        | RemoveWatcher IssueId Integer
        deriving (Show)

-- ID Values
-- | A Project's Id is an Integer.
type ProjectId      = Integer
-- | A Project's Identifier is a String.
type ProjectIdent   = String
-- | An Issue's Id is an Integer.
type IssueId        = Integer
-- | A Version's Id is an Integer.
type VersionId      = Integer


-- | A Redmine Project
data Project = Project
        { projectId          :: ProjectId       -- ^ The Project's ID Number
        , projectName        :: String          -- ^ The Project's Name
        , projectIdentifier  :: ProjectIdent    -- ^ The Project's Identifier
        , projectDescription :: String          -- ^ The Project's Description
        , projectCreated     :: String          -- ^ The Date the Project Was Created
        , projectUpdated     :: String          -- ^ The Date the Project Was Last Updated
        } deriving (Show)

-- | A List of Redmine Projects
newtype Projects = Projects [Project] deriving (Show)

instance FromJSON Project where
        parseJSON (Object v) =
                Project <$> v .: "id"
                        <*> v .: "name"
                        <*> v .: "identifier"
                        <*> v .: "description"
                        <*> v .: "created_on"
                        <*> v .: "updated_on"
        parseJSON _          = fail "Unable to parse Project JSON Object"

instance FromJSON Projects where
        parseJSON (Object v) = do
                projectArray <- v .: "projects"
                projectsList <- mapM parseJSON projectArray
                return $ Projects projectsList
        parseJSON _          = fail "Unable to parse Projects JSON Object"

-- | A Redmine Issue
data Issue = Issue
        { issueId            :: IssueId
        , issueTracker       :: String
        , issueStatus        :: String
        , issuePriority      :: String
        , issueAuthor        :: String
        , issueAssignedTo    :: Maybe String
        , issueCategory      :: Maybe String
        , issueVersion       :: Maybe String
        , issueSubject       :: String
        , issueDescription   :: String
        , issueDoneRatio     :: Integer
        , issueCreated       :: String
        , issueUpdated       :: String
        , issueStartDate     :: Maybe String
        , issueDueDate       :: Maybe String
        } deriving (Show)

-- | A List of Redmine Issues
newtype Issues = Issues [Issue] deriving (Show)

instance FromJSON Issue where
        parseJSON (Object r) = do
                (Object v) <- r .: "issue"
                Issue <$> v .: "id"
                      <*> v `grabName` "tracker"
                      <*> v `grabName` "status"
                      <*> v `grabName` "priority"
                      <*> v `grabName` "author"
                      <*> v `maybeGrabName` "assigned_to"
                      <*> v `maybeGrabName` "category"
                      <*> v `maybeGrabName` "fixed_version"
                      <*> v .: "subject"
                      <*> v .:? "description" .!= ""
                      <*> v .: "done_ratio"
                      <*> v .: "created_on"
                      <*> v .: "updated_on"
                      <*> v .:? "start_date"
                      <*> v .:? "due_date"
        parseJSON _          = fail "Unable to parse Issue JSON Object"

instance FromJSON Issues where
        parseJSON (Object v) = do
                issueArray  <- v .: "issues"
                -- Individual issues from a Redmine instance are wrapped in
                -- an Object with a single "issue" key. To parse the list
                -- of Issues, we need to emulate this before we parse each
                -- individual Issue.
                issuesList  <- mapM (\i -> parseJSON $ object ["issue" .= Object i])
                                   issueArray
                return $ Issues issuesList
        parseJSON _          = fail "Unable to parse Issues JSON Object"

-- | An Issue Status
data Status = Status
        { statusId          :: Integer
        , statusName        :: String
        , statusIsClosed    :: Bool
        , statusIsDefault   :: Bool
        } deriving (Show)

-- | A List of Redmine Issues
newtype Statuses = Statuses [Status] deriving (Show)

instance FromJSON Status where
        parseJSON (Object v) =
            Status <$> v .: "id"
                   <*> v .: "name"
                   <*> v .:? "is_closed" .!= False
                   <*> v .:? "is_default" .!= False
        parseJSON _          = fail "Unable to parse Status JSON Object"

instance FromJSON Statuses where
        parseJSON (Object v) = do
                statusArray <- v .: "issue_statuses"
                statusList  <- mapM parseJSON statusArray
                return $ Statuses statusList
        parseJSON _          = fail "Unable to parse Statuses JSON Object"

-- | A Redmine User
data User = User
        { userId            :: Integer
        , userLogin         :: String
        } deriving (Show)

instance FromJSON User where
        parseJSON (Object r) = do (Object v) <- r .: "user"
                                  User <$> v .: "id"
                                       <*> v .: "login"
        parseJSON _          = fail "Unable to parse User JSON Object."

-- | A Version
data Version = Version
        { versionId             :: VersionId
        , versionProjectId      :: ProjectId
        , versionName           :: String
        , versionDescription    :: String
        , versionDueDate        :: Maybe String
        , versionStatus         :: String
        } deriving (Show)

-- | A List of Redmine Versions
newtype Versions = Versions [Version] deriving (Show)

instance FromJSON Version where
        parseJSON (Object r) = do
            Object v <- r .: "version"
            Version <$> v .: "id"
                    <*> v `grabId` "project"
                    <*> v .: "name"
                    <*> v .: "description"
                    <*> v .:? "due_date"
                    <*> v .: "status"
        parseJSON _          = fail "Unable to parse Version JSON Object."

instance FromJSON Versions where
        parseJSON (Object v) = do
                versionArray <- v .: "versions"
                -- Individual versions from a Redmine instance are wrapped
                -- in an Object with a single "version" key. To parse the
                -- list we need to emulate this before we parse each
                -- individual version
                versionList  <- mapM (\i -> parseJSON $ object ["version" .= Object i])
                                   versionArray
                return $ Versions versionList
        parseJSON _          = fail "Unable to parse Versions JSON Object"

-- | A Time Entry Activity
data Activity = Activity
        { activityId            :: Integer
        , activityName          :: String
        , activityIsDefault     :: Bool
        } deriving (Show)

-- | A List of Time Entry Activities
newtype Activities = Activities [Activity]

instance FromJSON Activity where
        parseJSON (Object v) =
            Activity <$> v .: "id"
                     <*> v .: "name"
                     <*> v .:? "is_default" .!= False
        parseJSON _          = fail "Unable to parse Activity JSON Object."

instance FromJSON Activities where
        parseJSON (Object v) = do
                activityArray <- v .: "time_entry_activities"
                activityList  <- mapM parseJSON activityArray
                return $ Activities activityList
        parseJSON _          = fail "Unable to parse Activities JSON Object"

-- | A Redmine Tracker
data Tracker = Tracker
        { trackerId             :: Integer
        , trackerName           :: String
        } deriving (Show)

-- | A List of Trackers
newtype Trackers = Trackers [Tracker]

instance FromJSON Tracker where
        parseJSON (Object v) =
            Tracker <$> v .: "id"
                    <*> v .: "name"
        parseJSON _          = fail "Unable to parse Tracker JSON Object."

instance FromJSON Trackers where
        parseJSON (Object v) = do
                trackerArray <- v .: "trackers"
                trackerList  <- mapM parseJSON trackerArray
                return $ Trackers trackerList
        parseJSON _          = fail "Unable to parse Trackers JSON Object"

-- | A Redmine Issue Priority
data Priority = Priority
        { priorityId             :: Integer
        , priorityName           :: String
        , priorityIsDefault      :: Bool
        } deriving (Show)

-- | A List of Priorities
newtype Priorities = Priorities [Priority]

instance FromJSON Priority where
        parseJSON (Object v) =
            Priority <$> v .: "id"
                     <*> v .: "name"
                     <*> v .:? "is_default" .!= False
        parseJSON _          = fail "Unable to parse Issue Priority JSON Object."

instance FromJSON Priorities where
        parseJSON (Object v) = do
                array       <- v .: "issue_priorities"
                list        <- mapM parseJSON array
                return $ Priorities list
        parseJSON _         = fail "Unable to parse Issue Priorities JSON Object"

-- Utils
-- | Retrieve the 'name' attribute of an Object nested in an Object.
grabName :: FromJSON a => AT.Object -> T.Text -> AT.Parser a
obj `grabName` attr         = obj .: attr >>= \(Object o) -> o .: "name"
-- | Retrieve the 'id' attribute of an Object nested in an Object.
grabId :: FromJSON a => AT.Object -> T.Text -> AT.Parser a
obj `grabId` attr           = obj .: attr >>= \(Object o) -> o .: "id"

-- | 'grabName' for optional attributes
maybeGrabName :: FromJSON a => AT.Object -> T.Text -> AT.Parser (Maybe a)
obj `maybeGrabName` attr    = do attrObj <- obj .:? attr
                                 case attrObj of
                                     Nothing         -> return Nothing
                                     Just (Object o) -> fmap Just $ o .: "name"
                                     Just _          -> return Nothing
