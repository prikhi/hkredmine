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
        ( EndPoint(..)
        , ProjectId
        , Project(..)
        , Projects(..)
        , IssueId
        , Issue(..)
        , Issues(..)
        , Status(..)
        , Statuses(..)
        ) where

import Control.Applicative  ((<$>), (<*>))
import Data.Aeson           ((.:), (.:?), (.=), (.!=), object,
                             FromJSON, parseJSON, Value(Object))



-- | API EndPoints
data EndPoint    =
          GetProjects
        | GetStatuses
        | GetIssues
        | GetIssue IssueId
        | UpdateIssue IssueId
        deriving (Show)

-- | A Project's Id is an Integer.
type ProjectId = Integer

-- | A Redmine Project
data Project = Project
        { projectId          :: ProjectId  -- ^ The Project's ID Number
        , projectName        :: String   -- ^ The Project's Name
        , projectIdentifier  :: String   -- ^ The Project's Identifier
        , projectDescription :: String   -- ^ The Project's Description
        , projectCreated     :: String   -- ^ The Date the Project Was Created
        , projectUpdated     :: String   -- ^ The Date the Project Was Last Updated
        } deriving (Show)

-- | A List of Redmine Projects
newtype Projects = Projects [Project] deriving (Show)

-- | A Issue's Id is an Integer.
type IssueId = Integer

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

-- | An Issue Status
data Status = Status
        { statusId         :: Integer
        , statusName       :: String
        , statusIsClosed   :: Bool
        , statusIsDefault  :: Bool
        } deriving (Show)

-- | A List of Redmine Issues
newtype Statuses = Statuses [Status] deriving (Show)

-- JSON Parsing Instances
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
                      <*> v .: "description"
                      <*> v .: "done_ratio"
                      <*> v .: "created_on"
                      <*> v .: "updated_on"
                      <*> v .:? "start_date"
                      <*> v .:? "due_date"
                where
                      -- Retrieve the 'name' attribute of an Object nested
                      -- in an Object.
                      obj `grabName` attr      = obj .: attr >>=
                                            \(Object o) -> o .: "name"
                      -- 'grabName' with optional attributes
                      obj `maybeGrabName` attr = do
                                            attrObj <- obj .:? attr
                                            case attrObj of
                                                Nothing         -> return Nothing
                                                Just (Object o) -> fmap Just $ o .: "name"
                                                Just _          -> return Nothing
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
        parseJSON _          = fail "Unable to parse Projects JSON Object"

