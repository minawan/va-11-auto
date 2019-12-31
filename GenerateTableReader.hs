{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.IO as I hiding (putStrLn)
import GHC.Generics

jsonFile :: FilePath
jsonFile = "ScreenElement.json"

outputFile :: FilePath
outputFile = "GenerateScreenElement.hs"

data Table =
  Table { tableName :: !Text
        , tableColumns :: ![Column]
        } deriving (Generic, Show)

data Column =
  Column { columnName :: !Text
         , columnType :: !Text
         } deriving (Generic, Show)

instance FromJSON Table
instance FromJSON Column

getRawJSON :: FilePath -> IO B.ByteString
getRawJSON = B.readFile

convertColumnsToHaskell :: [Column] -> Text
convertColumnsToHaskell columns =
    Text.concat . List.intersperse separator $ map declare columns
  where
    separator = "\n  , "
    declare (Column columnName columnType) =
      Text.concat [columnName, " :: !", columnType]

-- Convert varName and a list [(columnName = name_i, columnType = _)] to:
--   varName .: "name_1" [<*> varName .: "name_i"]*
getParseNamedRecordInApplicativeForm :: [Column] -> Text -> Text
getParseNamedRecordInApplicativeForm columns varName =
    Text.concat . List.intersperse separator $ map project names
  where
    separator = " <*> "
    project component = Text.concat [ varName, " .: \"", component, "\""]
    names = map columnName columns

convertToHaskell :: Table -> Text
convertToHaskell (Table tableName tableColumns) =
    Text.unlines
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , ""
      , "import Data.Csv"
      , "import qualified Data.ByteString.Lazy as B"
      , "import Data.Vector (Vector)"
      , ""
      , Text.concat ["data ", tableName, " = ", tableName]
      , Text.concat ["  { ", convertColumnsToHaskell tableColumns ]
      , "  } deriving (Show)"
      , ""
      , Text.concat ["instance FromNamedRecord ", tableName, " where"]
      , Text.concat [ "  parseNamedRecord record = ", tableName, " <$> "
                    , getParseNamedRecordInApplicativeForm tableColumns "record"
                    ]
      , ""
      , "main :: IO ()"
      , "main = do"
      , "  csvData <- B.readFile \"ScreenElement.csv\""
      , Text.concat [ "  let decodedCsvData = decodeByName csvData"
                    , " :: Either String (Header, Vector ", tableName, ")"
                    ]
      , "  case decodedCsvData of"
      , "    Left err -> putStrLn err"
      , "    Right (_, val) -> putStrLn $ show val"
      ]

main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String Table)
  case res of
    Left err -> putStrLn err
    Right table -> I.writeFile outputFile $ convertToHaskell table
