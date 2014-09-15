{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables,
             FlexibleContexts #-}
{-|
Module          : HKRedmine
Description     : A Redmine CLI Client
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX
-}
module Main where

import Control.Monad            (void, when)
import Control.Monad.IO.Class   (liftIO)
import System.Environment       (getArgs)
import System.Directory         (doesFileExist, getAppUserDataDirectory,
                                 createDirectoryIfMissing)

import Web.HTTP.Redmine


-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main              = do
        _ <- initializeApp
        args <- liftIO getArgs
        defCfg <- defaultRedmineConfig
        let cfg = defCfg { redURL = ""
                         , redAPI = "" }
        case args of
            ["print", "projects"]           -> void $ runRedmine cfg printProjects
            ["print", "project", projectID] -> void $ runRedmine cfg $ printProject
                                                    $ read projectID
            ["print", "issues"]             -> void $ runRedmine cfg printIssues
            ["print", "issue", issueID]     -> error "Not Yet Implemented"
            ["track", "project", projectID] -> trackProject $ read projectID
            ["track", "issue", issueID]     -> error "Not Yet Implemented"
            _                               -> liftIO printUsage

-- | Create the Data Directory & Files for the Application
initializeApp :: IO ()
initializeApp = do
        appDir <- getAppUserDataDirectory "hkredmine"
        _      <- createDirectoryIfMissing True appDir
        let projectFile = appDir ++ "/project"
        projectFileExists <- doesFileExist projectFile
        when (not projectFileExists) (writeFile projectFile "")

-- | Print the Program's Usage Text
printUsage :: IO ()
printUsage        = do
        let message = [ "HKRedmine - Redmine CLI Client"
                      , ""
                      , "Usage:"
                      , "hkredmine command <args>"
                      , ""
                      , "Commands:"
                      , "print projects           -- Print All Projects"
                      , "print project <id>       -- Print the Details of a Specific Project"
                      , "print issues             -- Print All Issues of the Tracked Project"
                      , "print myissues           -- Print Your Issues of the Tracked Project"
                      , "track project <id>       -- Track the Specified Project"
                      , ""
                      ]
        mapM_ putStrLn message

-- | Print All Projects
printProjects :: Redmine ()
printProjects = do Projects ps <- getProjects
                   liftIO . putStrLn . projectsTable $ ps

-- | Print A Single Project
printProject :: Integer -> Redmine ()
printProject projectID = do Projects ps <- getProjects
                            void . liftIO . sequence $
                                map (\p -> when (projectId p == projectID) $
                                                putStrLn . projectDetail $ p) ps

-- | Print All Issues of a Project
printIssues :: Redmine ()
printIssues = do
        projectID <- liftIO getTrackedProject
        Issues is <- getAllIssues projectID
        liftIO . putStrLn . issuesTable $ is

-- | Track A Project By Writing It's ID to a File
trackProject :: Integer -> IO ()
trackProject projectID = do
        appDir <- getAppUserDataDirectory "hkredmine"
        writeFile (appDir ++ "/project") $ show projectID ++ "\n"

getTrackedProject :: IO Integer
getTrackedProject = do
        appDir <- getAppUserDataDirectory "hkredmine"
        projectID <- readFile (appDir ++ "/project")
        return $ read projectID
