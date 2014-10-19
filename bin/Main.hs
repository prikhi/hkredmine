{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : Main
Description     : A Redmine CLI Client
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX
-}
module Main (main) where


import Control.Monad            (join)
import Control.Monad.Except     (runExceptT)
import Control.Monad.IO.Class   (liftIO)
import Data.ConfigFile          (get, emptyCP, readfile, has_section, sections,
                                 CPErrorData(..))
import Data.Maybe               (fromJust, isJust)
import System.Console.CmdArgs   (cmdArgs_)
import System.Exit              (exitFailure, exitSuccess)
import System.Directory         (createDirectoryIfMissing, getHomeDirectory)

import Web.HTTP.Redmine

import Main.Utils
import Main.CLI

-- | Parse Any Passed Arguments to Figure Out What to Do
main :: IO ()
main                    = do
        argMode         <- cmdArgs_ hkredmine
        cfg             <- initializeApp
        let action      = dispatch argMode
        result          <- runRedmine cfg action
        case result of
            Right _     -> exitSuccess
            Left err    -> putStrLn err >> exitFailure

-- | Create the data directory for the application, read the config file
-- and return a 'RedmineConfig'.
initializeApp :: IO RedmineConfig
initializeApp           = do
        appDir          <- getAppDir
        _               <- createDirectoryIfMissing True appDir
        getConfig

-- | Read the User's config file and attempt to generate a 'RedmineConfig'.
getConfig :: IO RedmineConfig
getConfig               = do
        defCfg          <- defaultRedmineConfig
        homeDir         <- getHomeDirectory
        let configPath  = homeDir ++ "/.hkredminerc"
        mayAcc          <- getAccount
        eithCfg         <- runExceptT $ do
                cp      <- join $ liftIO $ readfile emptyCP configPath
                let acc = if isJust mayAcc && has_section cp (fromJust mayAcc)
                          then fromJust mayAcc
                          else head $ sections cp  ++ ["DEFAULT"]
                url     <- get cp acc "url"
                key     <- get cp acc "apikey"
                return $ defCfg { redURL = url, redAPI = key }
        case eithCfg of
            Right cfg               -> return cfg
            Left (NoOption o, _)    -> putStrLn ("Could not read " ++ o ++ " Option.")
                                    >> exitFailure
            Left (NoSection s, _)   -> putStrLn ("Could not find a " ++ s ++ " Account.")
                                    >> exitFailure
            Left (ParseError _, _)  -> putStrLn "Could not parse config file."
                                    >> exitFailure
            Left _                  -> putStrLn "Encountered an error reading the config file."
                                    >> exitFailure
