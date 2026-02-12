{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeAnalyzer
    ( CodeAnalysis(..)
    , analyzeCode
    , formatCode
    , lintCode
    ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data CodeAnalysis = CodeAnalysis
    { codeLines :: Int
    , codeFunctions :: Int
    , codeTypes :: Int
    , codeModules :: Int
    , language :: Text
    , purityScore :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON CodeAnalysis where
    toJSON analysis = object
        [ "lines" .= codeLines analysis
        , "functions" .= codeFunctions analysis
        , "types" .= codeTypes analysis
        , "modules" .= codeModules analysis
        , "language" .= language analysis
        , "purity_score" .= purityScore analysis
        ]

analyzeCode :: Text -> CodeAnalysis
analyzeCode code =
    let
        lns = T.lines code
        lineCount = length lns
        functionCount = countOccurrences "::" code
        typeCount = countOccurrences "data " code + countOccurrences "type " code + countOccurrences "newtype " code
        moduleCount = countOccurrences "module " code
        
        -- Haskell purity heuristic: ratio of pure functions to IO functions
        ioCount = countOccurrences "IO " code
        purity = if functionCount > 0
                 then (fromIntegral (functionCount - ioCount) / fromIntegral functionCount) * 100.0
                 else 100.0
    in
        CodeAnalysis
            { codeLines = lineCount
            , codeFunctions = functionCount
            , codeTypes = typeCount
            , codeModules = moduleCount
            , language = "haskell"
            , purityScore = purity
            }

countOccurrences :: Text -> Text -> Int
countOccurrences pattern text =
    length $ T.breakOnAll pattern text

formatCode :: Text -> Either Text Text
formatCode code =
    -- In production, this would use stylish-haskell or brittany
    Right $ T.unlines $ map T.strip $ T.lines code

lintCode :: Text -> Either Text [Text]
lintCode code =
    -- In production, this would use HLint
    let warnings = []
        hasUndefinedUsage = T.isInfixOf "undefined" code
        hasPartialFunctions = or
            [ T.isInfixOf "head" code
            , T.isInfixOf "tail" code
            , T.isInfixOf "!!" code
            ]
        
        allWarnings = warnings
            ++ (if hasUndefinedUsage then ["Avoid using 'undefined' in production code"] else [])
            ++ (if hasPartialFunctions then ["Avoid partial functions; use safe alternatives"] else [])
    in
        Right allWarnings
