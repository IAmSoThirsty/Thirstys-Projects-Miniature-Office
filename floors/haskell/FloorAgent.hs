{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FloorAgent
    ( FloorAgent(..)
    , AgentId
    , Role(..)
    , Capability(..)
    , createAgent
    , agentToJSON
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics (Generic)

type AgentId = Text

data Role
    = Architect
    | Implementer
    | Reviewer
    | Tester
    | SecurityExpert
    | Manager
    deriving (Show, Eq, Generic)

instance ToJSON Role where
    toJSON Architect = "Architect"
    toJSON Implementer = "Implementer"
    toJSON Reviewer = "Reviewer"
    toJSON Tester = "Tester"
    toJSON SecurityExpert = "SecurityExpert"
    toJSON Manager = "Manager"

instance FromJSON Role

data Capability
    = Design
    | Implementation
    | CodeReview
    | Testing
    | SecurityAudit
    | ProjectManagement
    deriving (Show, Eq, Generic)

instance ToJSON Capability where
    toJSON Design = "design"
    toJSON Implementation = "implementation"
    toJSON CodeReview = "code_review"
    toJSON Testing = "testing"
    toJSON SecurityAudit = "security_audit"
    toJSON ProjectManagement = "project_management"

instance FromJSON Capability

data FloorAgent = FloorAgent
    { agentId :: AgentId
    , agentName :: Text
    , agentRole :: Role
    , capabilities :: [Capability]
    } deriving (Show, Eq, Generic)

instance ToJSON FloorAgent where
    toJSON agent = object
        [ "agent_id" .= agentId agent
        , "name" .= agentName agent
        , "role" .= agentRole agent
        , "capabilities" .= capabilities agent
        ]

instance FromJSON FloorAgent where
    parseJSON = JSON.withObject "FloorAgent" $ \v -> FloorAgent
        <$> v .: "agent_id"
        <*> v .: "name"
        <*> v .: "role"
        <*> v .: "capabilities"

createAgent :: AgentId -> Text -> Role -> [Capability] -> FloorAgent
createAgent = FloorAgent

agentToJSON :: FloorAgent -> JSON.Value
agentToJSON = toJSON
