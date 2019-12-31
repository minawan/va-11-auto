{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
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

main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String Table)
  case res of
    Left err -> putStrLn err
    Right table -> I.writeFile outputFile . Text.pack $ show table
