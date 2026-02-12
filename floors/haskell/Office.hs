{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Office
    ( Office(..)
    , OfficeType(..)
    , allOffices
    , getOfficeDescription
    ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

data OfficeType
    = ArchitectureOffice
    | ImplementationOffice
    | ReviewOffice
    | TestOffice
    | SecurityOffice
    | ManagerOffice
    deriving (Show, Eq, Enum, Bounded, Generic)

data Office = Office
    { officeType :: OfficeType
    , officeName :: Text
    , description :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON Office where
    toJSON office = object
        [ "type" .= show (officeType office)
        , "name" .= officeName office
        , "description" .= description office
        ]

allOffices :: [Office]
allOffices =
    [ Office ArchitectureOffice "Architecture Office" "Type-safe design and system architecture"
    , Office ImplementationOffice "Implementation Office" "Pure functional implementation with monadic effects"
    , Office ReviewOffice "Review Office" "Code review with property-based testing mindset"
    , Office TestOffice "Test Office" "QuickCheck and HUnit testing"
    , Office SecurityOffice "Security Office" "Type-level security and safe API design"
    , Office ManagerOffice "Manager Office" "Project coordination and dependency management"
    ]

getOfficeDescription :: OfficeType -> Text
getOfficeDescription ArchitectureOffice = "Type-safe design and system architecture"
getOfficeDescription ImplementationOffice = "Pure functional implementation with monadic effects"
getOfficeDescription ReviewOffice = "Code review with property-based testing mindset"
getOfficeDescription TestOffice = "QuickCheck and HUnit testing"
getOfficeDescription SecurityOffice = "Type-level security and safe API design"
getOfficeDescription ManagerOffice = "Project coordination and dependency management"
