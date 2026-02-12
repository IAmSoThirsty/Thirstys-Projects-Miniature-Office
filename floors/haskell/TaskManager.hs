{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TaskManager
    ( Task(..)
    , TaskId
    , TaskStatus(..)
    , TaskState
    , emptyTaskState
    , createTask
    , getTask
    , getAllTasks
    , updateTaskStatus
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)

type TaskId = Text

data TaskStatus
    = Pending
    | InProgress
    | Completed
    | Failed
    deriving (Show, Eq, Generic)

instance ToJSON TaskStatus where
    toJSON Pending = "pending"
    toJSON InProgress = "in_progress"
    toJSON Completed = "completed"
    toJSON Failed = "failed"

instance FromJSON TaskStatus

data Task = Task
    { taskId :: TaskId
    , taskTitle :: Text
    , taskStatus :: TaskStatus
    , assignedTo :: Text
    , createdAt :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON Task where
    toJSON task = object
        [ "task_id" .= taskId task
        , "title" .= taskTitle task
        , "status" .= taskStatus task
        , "assigned_to" .= assignedTo task
        , "created_at" .= createdAt task
        ]

instance FromJSON Task where
    parseJSON = JSON.withObject "Task" $ \v -> Task
        <$> v .: "task_id"
        <*> v .: "title"
        <*> v .: "status"
        <*> v .: "assigned_to"
        <*> v .: "created_at"

type TaskState = Map TaskId Task

emptyTaskState :: TaskState
emptyTaskState = Map.empty

formatUTCTime :: UTCTime -> Text
formatUTCTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

createTask :: MonadIO m => TaskId -> Text -> Text -> m Task
createTask tid title assignee = do
    now <- liftIO getCurrentTime
    return Task
        { taskId = tid
        , taskTitle = title
        , taskStatus = Pending
        , assignedTo = assignee
        , createdAt = formatUTCTime now
        }

getTask :: TaskId -> TaskState -> Maybe Task
getTask = Map.lookup

getAllTasks :: TaskState -> [Task]
getAllTasks = Map.elems

updateTaskStatus :: TaskId -> TaskStatus -> TaskState -> TaskState
updateTaskStatus tid status = Map.adjust (\t -> t { taskStatus = status }) tid
