{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : DepartmentFloor
Description : Haskell Department Floor - Floor 20
Copyright   : (c) Miniature Office, 2024
License     : MIT

FLOOR 20 - HASKELL JURISDICTION
Domain: Pure functional programming, Type-safe systems, Compilers
Architectural Law: Type safety > runtime checks, No partial functions, Monadic error handling
-}

module Main (main) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.IO (hFlush, hPutStrLn, stdout, stderr)

import FloorAgent (FloorAgent, AgentId, Role, Capability, createAgent)
import TaskManager (Task, TaskState, emptyTaskState, createTask, getAllTasks)
import CodeAnalyzer (analyzeCode, formatCode, lintCode)


-- Floor State
data FloorState = FloorState
    { floorAgents :: Map AgentId FloorAgent
    , floorTasks :: TaskState
    } deriving (Show, Generic)

emptyFloorState :: FloorState
emptyFloorState = FloorState Map.empty emptyTaskState

type FloorM = StateT FloorState IO

-- JSON-RPC Request
data RPCRequest = RPCRequest
    { rpcMethod :: Text
    , rpcParams :: Maybe Value
    } deriving (Show, Generic)

instance FromJSON RPCRequest where
    parseJSON = JSON.withObject "RPCRequest" $ \v -> RPCRequest
        <$> v .: "method"
        <*> v .:? "params"

-- JSON-RPC Response
data RPCResponse = RPCResponse
    { responseData :: Value
    } deriving (Show, Generic)

instance ToJSON RPCResponse where
    toJSON (RPCResponse val) = val

-- Floor Info
data FloorInfo = FloorInfo
    { floorNumber :: Int
    , floorLanguage :: Text
    , domain :: Text
    , architecturalLaw :: Text
    , securityDoctrine :: Text
    , offices :: [Text]
    , agentCount :: Int
    , taskCount :: Int
    , agents :: [FloorAgent]
    , tasks :: [Task]
    } deriving (Show, Generic)

instance ToJSON FloorInfo where
    toJSON info = object
        [ "floor_number" .= floorNumber info
        , "language" .= floorLanguage info
        , "domain" .= domain info
        , "architectural_law" .= architecturalLaw info
        , "security_doctrine" .= securityDoctrine info
        , "offices" .= offices info
        , "agent_count" .= agentCount info
        , "task_count" .= taskCount info
        , "agents" .= agents info
        , "tasks" .= tasks info
        ]

-- Request Parameters
data AddAgentParams = AddAgentParams
    { paramAgentId :: Text
    , paramName :: Text
    , paramRole :: Role
    , paramCapabilities :: [Capability]
    } deriving (Show, Generic)

instance FromJSON AddAgentParams where
    parseJSON = JSON.withObject "AddAgentParams" $ \v -> AddAgentParams
        <$> v .: "agent_id"
        <*> v .: "name"
        <*> v .: "role"
        <*> v .: "capabilities"

data CreateTaskParams = CreateTaskParams
    { paramTaskId :: Text
    , paramTitle :: Text
    , paramAssignedTo :: Text
    } deriving (Show, Generic)

instance FromJSON CreateTaskParams where
    parseJSON = JSON.withObject "CreateTaskParams" $ \v -> CreateTaskParams
        <$> v .: "task_id"
        <*> v .: "title"
        <*> v .: "assigned_to"

data ProcessCodeParams = ProcessCodeParams
    { paramCode :: Text
    , paramOperation :: Text
    } deriving (Show, Generic)

instance FromJSON ProcessCodeParams where
    parseJSON = JSON.withObject "ProcessCodeParams" $ \v -> ProcessCodeParams
        <$> v .: "code"
        <*> v .: "operation"

-- Handlers
handleGetInfo :: FloorM Value
handleGetInfo = do
    state <- get
    let agentList = Map.elems (floorAgents state)
        taskList = getAllTasks (floorTasks state)
        info = FloorInfo
            { floorNumber = 20
            , floorLanguage = "haskell"
            , domain = "Pure functional programming, Type-safe systems, Compilers"
            , architecturalLaw = "Type safety > runtime checks, No partial functions, Monadic error handling"
            , securityDoctrine = "Type-level security, Safe API design, No unsafe operations"
            , offices = ["Architecture Office", "Implementation Office", "Review Office", 
                         "Test Office", "Security Office", "Manager Office"]
            , agentCount = Map.size (floorAgents state)
            , taskCount = length taskList
            , agents = agentList
            , tasks = taskList
            }
    return $ toJSON info

handleAddAgent :: Value -> FloorM Value
handleAddAgent params = do
    case JSON.fromJSON params of
        JSON.Error err -> return $ object ["status" .= ("error" :: Text), "message" .= err]
        JSON.Success (AddAgentParams aid name role caps) -> do
            let agent = createAgent aid name role caps
            modify $ \s -> s { floorAgents = Map.insert aid agent (floorAgents s) }
            return $ object ["status" .= ("success" :: Text), "agent" .= agent]

handleCreateTask :: Value -> FloorM Value
handleCreateTask params = do
    case JSON.fromJSON params of
        JSON.Error err -> return $ object ["status" .= ("error" :: Text), "message" .= err]
        JSON.Success (CreateTaskParams tid title assignee) -> do
            task <- createTask tid title assignee
            modify $ \s -> s { floorTasks = Map.insert tid task (floorTasks s) }
            return $ object ["status" .= ("success" :: Text), "task" .= task]

handleProcessCode :: Value -> FloorM Value
handleProcessCode params = do
    case JSON.fromJSON params of
        JSON.Error err -> return $ object ["status" .= ("error" :: Text), "message" .= err]
        JSON.Success (ProcessCodeParams code op) ->
            case op of
                "analyze" -> do
                    let analysis = analyzeCode code
                    return $ object ["status" .= ("success" :: Text), "analysis" .= analysis]
                "format" ->
                    case formatCode code of
                        Left err -> return $ object ["status" .= ("error" :: Text), "message" .= err]
                        Right formatted -> return $ object
                            [ "status" .= ("success" :: Text)
                            , "formatted" .= True
                            , "code" .= formatted
                            ]
                "lint" ->
                    case lintCode code of
                        Left err -> return $ object ["status" .= ("error" :: Text), "message" .= err]
                        Right warnings -> return $ object
                            [ "status" .= ("success" :: Text)
                            , "warnings" .= warnings
                            ]
                _ -> return $ object ["status" .= ("error" :: Text), "message" .= ("Unknown operation: " <> op)]

handleRequest :: RPCRequest -> FloorM Value
handleRequest (RPCRequest method maybeParams) =
    let params = fromMaybe (object []) maybeParams
    in case method of
        "get_info" -> handleGetInfo
        "add_agent" -> handleAddAgent params
        "create_task" -> handleCreateTask params
        "process_code" -> handleProcessCode params
        _ -> return $ object ["status" .= ("error" :: Text), "message" .= ("Unknown method: " <> method)]

processRequest :: Text -> FloorM ()
processRequest line = do
    case JSON.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 line) of
        Left err -> do
            let response = object ["status" .= ("error" :: Text), "message" .= ("Invalid JSON: " <> T.pack err)]
            liftIO $ BSL.putStrLn (JSON.encode response)
            liftIO $ hFlush stdout
        Right req -> do
            response <- handleRequest req
            liftIO $ BSL.putStrLn (JSON.encode response)
            liftIO $ hFlush stdout

mainLoop :: FloorM ()
mainLoop = forever $ do
    line <- liftIO TIO.getLine
    processRequest line

main :: IO ()
main = do
    hPutStrLn stderr "Haskell Department Floor (Floor 20) - Ready"
    hPutStrLn stderr "Domain: Pure functional programming, Type-safe systems, Compilers"
    hPutStrLn stderr "Architectural Law: Type safety > runtime checks, No partial functions"
    hPutStrLn stderr "Offices: Architecture, Implementation, Review, Test, Security, Manager"
    hFlush stderr
    
    evalStateT mainLoop emptyFloorState
