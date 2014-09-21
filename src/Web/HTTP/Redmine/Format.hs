{-|
-
- This Module contains functions for prettifying 'Redmine Types' for
- display.
-
-}
module Web.HTTP.Redmine.Format
        ( projectsTable
        , issuesTable
        , versionTable
        , projectDetail
        , versionDetail
        ) where

import Data.List    (intercalate)
import Data.Maybe   (fromMaybe)

import Web.HTTP.Redmine.FormatTable
import Web.HTTP.Redmine.Types

-- | Create a Table of Projects with ID, Name, Identifier, Description
-- & Updated Columns
projectsTable :: [Project] -> String
projectsTable = showTable [ ColDesc center "ID" left (show . projectId)
                          , ColDesc center "Identifier" left projectIdentifier
                          , ColDesc center "Name" left projectName
                          , ColDesc center "Description" left (takeWhile (/= '\r') . projectDescription)
                          , ColDesc center "Updated" left (takeWhile (/= 'T') . projectUpdated)
                          ]

-- | Create a Table of Issues with an ID#, Tracker, Status, Priority,
-- Category & Subject.
issuesTable :: [Issue] -> String
issuesTable = showTable [ ColDesc center "ID" left $ show . issueId
                        , ColDesc center "Subject" left issueSubject
                        , ColDesc center "Status" left issueStatus
                        , ColDesc center "Priority" left issuePriority
                        , ColDesc center "Tracker" left issueTracker
                        , ColDesc center "Category" left $ fromMaybe "" . issueCategory
                        ]

-- | Create a Table of Versions.
versionTable :: [Version] -> String
versionTable = showTable [ ColDesc center "ID" left $ show . versionId
                         , ColDesc center "Name" left versionName
                         , ColDesc center "Status" left versionStatus
                         , ColDesc center "Description" left (takeWhile (/= '\r') . versionDescription)
                         , ColDesc center "Due Date" left $ fromMaybe "" . versionDueDate
                         ]

-- | Create a String Displaying a Project's Name, ID, Identifier,
-- Created/Updated Dates & Description
projectDetail :: Project -> String
projectDetail p = intercalate "\n"
        [ projectName p
        , horizLine '=' $ projectName p
        , ""
        , "Identifier:  " ++ projectIdentifier p
        , ""
        , "Created On:  " ++ projectCreated p
        , "Updated On:  " ++ projectUpdated p
        , ""
        , "Description:"
        , projectDescription p
        ]

-- | Create a string to desiplay the Id, Name, Description and Due Date of
-- a 'Version'.
versionDetail :: Version -> String
versionDetail v = intercalate "\n"
        [ vn
        , horizLine '=' vn
        , ""
        , "ID:      " ++ show (versionId v)
        , "Status:  " ++ versionStatus v
        , "Due:     " ++ fromMaybe "" (versionDueDate v)
        , ""
        , "Description:"
        , versionDescription v
        ]
        where vn = versionName v

-- | Create a horizontal line the length of another string(+1)
horizLine :: Char -> String -> String
horizLine c s = replicate (length s + 1) c
