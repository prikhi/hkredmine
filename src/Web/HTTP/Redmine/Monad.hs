{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This Module contains functions and types related to the 'Redmine' Monad.
-
-}
module Web.HTTP.Redmine.Monad
        ( Redmine
        , runRedmine
        , RedmineConfig(..)
        , defaultRedmineConfig
        , redmineLeft
        , redmineDecode
        , redmineMVar
        , redmineTakeMVar
        ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB     (ByteString)

import Control.Concurrent               (forkIO, putMVar, takeMVar, MVar,
                                         newEmptyMVar)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Logger             (runStderrLoggingT, LoggingT)
import Control.Monad.State              (evalStateT, StateT, get)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Either       (runEitherT, EitherT, left,
                                         hoistEither)
import Control.Monad.Trans.Resource     (runResourceT, ResourceT)
import Data.Aeson                       (FromJSON, eitherDecode)
import Network.HTTP.Conduit             (Manager, newManager, Response,
                                         conduitManagerSettings, responseBody)


-- | The Redmine Monad Holds the Configuration State and Allows Network and
-- Logging Actions
type Redmine a           = LoggingT (StateT RedmineConfig (ResourceT (EitherT String IO))) a

-- | Run a 'Redmine' Action
runRedmine :: MonadIO m => RedmineConfig -> Redmine a -> m (Either String a)
runRedmine config action = liftIO . runEitherT . runResourceT .
                          flip evalStateT config .  runStderrLoggingT $ action

-- | Represents the Current Redmine Configuration State
data RedmineConfig       = RedmineConfig
                         { -- | The User's API Key
                           redAPI    :: B.ByteString
                           -- | The Desired API URL
                         , redURL     :: String
                           -- | The Connection Manager for All Requests
                         , redManager :: Manager
                         }

-- | Creates a 'RedmineConfig' with a new Manager
defaultRedmineConfig :: MonadIO m => m RedmineConfig
defaultRedmineConfig     = do
        man <- liftIO $ newManager conduitManagerSettings
        return RedmineConfig { redAPI     = ""
                             , redURL     = ""
                             , redManager = man }

-- | Return an irrecoverable error in the 'Redmine' Monad.
redmineLeft :: String -> Redmine a
redmineLeft             = lift . lift . lift . left

-- | Decode a JSON response in the Redmine Monad.
redmineDecode :: FromJSON a => Response LB.ByteString -> Redmine a
redmineDecode = lift . lift . lift . hoistEither . eitherDecode . responseBody

-- | Execute a 'Redmine' action in a fork, putting the action's result in
-- an 'MVar'.
redmineMVar :: Redmine a ->  Redmine (MVar (Either String a))
redmineMVar a           = do
        cfg             <- get
        box             <- liftIO newEmptyMVar
        _               <- liftIO . forkIO $ runRedmine cfg a >>= putMVar box
        return box

-- | Pull an item out of a 'MVar' in the 'Redmine' monad.
redmineTakeMVar :: MVar (Either String a) -> Redmine a
redmineTakeMVar m       = do
        result          <- liftIO $ takeMVar m
        case result of
             Right r    -> return r
             Left l     -> redmineLeft l
