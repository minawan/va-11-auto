{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.IO as I hiding (putStrLn)
import GHC.Generics
import Text.Printf (printf)

jsonFile :: FilePath
jsonFile = "ScreenElement.json"

outputLibFile :: FilePath
outputLibFile = "ScreenElement.hs"

outputBinFile :: FilePath
outputBinFile = "GenerateScreenElement.hs"

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

convertToHaskellLib :: Table -> Text
convertToHaskellLib (Table tableName tableColumns) =
    Text.pack . unlines $
      [ "{-# LANGUAGE OverloadedStrings #-}"
      , "module ScreenElement where"
      , ""
      , "import Data.Csv"
      , ""
      , printf "data %s = %s" tableName tableName
      , printf "  { %s" (convertColumnsToHaskell tableColumns)
      , "  } deriving (Show)"
      , ""
      , printf "instance FromNamedRecord %s where" tableName
      , printf "  parseNamedRecord record = %s <$> %s" tableName
          (getParseNamedRecordInApplicativeForm tableColumns "record")
      ]

convertToHaskellBin :: Table -> Text
convertToHaskellBin (Table tableName tableColumns) =
    Text.pack . unlines $
      [ "import Data.Csv (Header, decodeByName)"
      , "import qualified Data.ByteString.Lazy as B"
      , "import Data.Vector (Vector)"
      , ""
      , "import ScreenElement"
      , ""
      , "main :: IO ()"
      , "main = do"
      , printf "  csvData <- B.readFile \"ScreenElement.csv\""
      , printf "  let decodedCsvData = decodeByName csvData :: Either String (Header, Vector %s)" tableName
      , "  case decodedCsvData of"
      , "    Left err -> putStrLn err"
      , "    Right (_, val) -> putStrLn $ show val"
      ]

main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String Table)
  case res of
    Left err -> putStrLn err
    Right table -> do
      I.writeFile outputLibFile $ convertToHaskellLib table
      I.writeFile outputBinFile $ convertToHaskellBin table
