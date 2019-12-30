{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Aeson 
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as I hiding (putStrLn)
import GHC.Generics

jsonFile :: FilePath
jsonFile = "VA-11_Cheat_Sheet.json"

outputFile :: FilePath
outputFile = "drink.py"

data DrinkAttribute = Flavor
                    | Kind
                    | Trait
                    | Price
                    | Recipe
                    deriving (Enum)

data DrinkFlavor = Bitter
                 | Bubbly
                 | Sour
                 | Spicy
                 | Sweet
                 deriving (Enum, Show)

data DrinkKind = Classic
               | Classy
               | Girly
               | Manly
               | Promo
               deriving (Enum, Show)

data DrinkTrait = Bland
                | Burning
                | Happy
                | Sobering
                | Soft
                | Strong
                | Vintage
                deriving (Enum, Show)

data Ingredient = Adelhyde
                | BronsonExtract
                | PowderedDelta
                | Flanergide
                | Karmotrine
                deriving (Enum)

data RecipeAction = AddIce
                  | Age
                  | Mix
                  | Blend
                  deriving (Enum)

data Drink =
  Drink { name :: !Text
        , flavor :: !Text
        , kind :: !Text
        , trait :: !Text
        , price :: Int
        , recipe :: DrinkRecipe
        } deriving (Generic, Show)

data DrinkRecipe =
  DrinkRecipe { adelhyde :: Int
              , bronsonExtract :: Int
              , powderedDelta :: Int
              , flanergide :: Int
              , karmotrine :: Int
              , addIce :: Bool
              , age :: Bool
              , mix :: Bool
              , blend :: Bool
              } deriving (Generic, Show)

instance Show DrinkAttribute where
  show Flavor = "flavor"
  show Kind = "kind"
  show Trait = "trait"
  show Price = "price"
  show Recipe = "recipe"

instance Show Ingredient where
  show Adelhyde = "adelhyde"
  show BronsonExtract = "bronson_extract"
  show PowderedDelta = "powdered_delta"
  show Flanergide = "flanergide"
  show Karmotrine = "karmotrine"

instance Show RecipeAction where
  show AddIce = "ice"
  show Age = "age"
  show Mix = "mix"
  show Blend = "blend"

instance FromJSON Drink
instance FromJSON DrinkRecipe

getRawJSON :: FilePath -> IO B.ByteString
getRawJSON = B.readFile

toText :: Show a => a -> Text
toText = Text.pack . show

constantsFromDrink :: [Text]
constantsFromDrink = map (Text.pack . show)
  [ Flavor
  , Kind
  , Trait
  , Price
  , Recipe
  ]

constantsFromRecipe :: [Text]
constantsFromRecipe =
    concat [ map toText ingredientConstants
           , map toText recipeActionConstants
           , map toText drinkFlavorConstants
           , map toText drinkKindConstants
           , map toText drinkTraitConstants
           ]
  where
    ingredientConstants =
      [ Adelhyde
      , BronsonExtract
      , PowderedDelta
      , Flanergide
      , Karmotrine
      ]
    recipeActionConstants =
      [ AddIce
      , Age
      , Mix
      , Blend
      ]
    drinkFlavorConstants =
      [ Bitter
      , Bubbly
      , Sour
      , Spicy
      , Sweet
      ]
    drinkKindConstants =
      [ Classic
      , Classy
      , Girly
      , Manly
      , Promo
      ]
    drinkTraitConstants =
      [ Bland
      , Burning
      , Happy
      , Sobering
      , Soft
      , Strong
      , Vintage
      ]

convertConstantToSymbol :: Text -> Text
convertConstantToSymbol "" = "NONE"
convertConstantToSymbol constant = Text.replace " " "_" $ Text.toUpper constant

initializeConstant :: Text -> Text
initializeConstant constant = Text.concat [symbol, " = '", constant, "'"]
  where symbol = convertConstantToSymbol constant

convertConstantsToInitializations :: [Text] -> [Text]
convertConstantsToInitializations = map initializeConstant

initializeConstants :: [Drink] -> [Text]
initializeConstants drinks = convertConstantsToInitializations constants
  where drinkNames = map name drinks
        constants = constantsFromDrink
                 ++ constantsFromRecipe
                 ++ drinkNames ++ [""]


boolToText :: Bool -> Text
boolToText True = "True"
boolToText False = "False"

convertDrinkRecipeToPythonDict :: DrinkRecipe -> Text
convertDrinkRecipeToPythonDict (DrinkRecipe adelhyde bronsonExtract powderedDelta flanergide karmotrine addIce age mix blend) =
    Text.concat
      [ "{", indent, convertConstantToSymbol $ toText Adelhyde, colon, toText adelhyde, comma
      , indent, convertConstantToSymbol $ toText BronsonExtract, colon, toText bronsonExtract, comma
      , indent, convertConstantToSymbol $ toText PowderedDelta, colon, toText powderedDelta, comma
      , indent, convertConstantToSymbol $ toText Flanergide, colon, toText flanergide, comma
      , indent, convertConstantToSymbol $ toText Karmotrine, colon, toText karmotrine, comma
      , indent, convertConstantToSymbol $ toText AddIce, colon, boolToText addIce, comma
      , indent, convertConstantToSymbol $ toText Age, colon, boolToText age, comma
      , indent, convertConstantToSymbol $ toText Mix, colon, boolToText mix, comma
      , indent, convertConstantToSymbol $ toText Blend, colon, boolToText blend, comma, "\n  }"
      ]
  where
    colon = ": "
    comma = ", "
    indent = "\n    "

convertToPythonDict :: Drink -> Text
convertToPythonDict (Drink name flavor kind trait price recipe) =
    Text.concat
      [ convertConstantToSymbol name, ": { "
      , indent, convertConstantToSymbol $ toText Flavor, colon, convertConstantToSymbol flavor, comma
      , indent, convertConstantToSymbol $ toText Kind, colon, convertConstantToSymbol kind, comma
      , indent, convertConstantToSymbol $ toText Trait, colon, convertConstantToSymbol trait, comma
      , indent, convertConstantToSymbol $ toText Price, colon, toText price, comma
      , indent, convertConstantToSymbol $ toText Recipe, colon, convertDrinkRecipeToPythonDict recipe, ",\n},"
      ]
  where
    colon = ": "
    comma = ", "
    indent = "\n  "

generatePythonDict :: [Drink] -> Text
generatePythonDict drinks =
  Text.concat [ Text.unlines $ initializeConstants drinks
              , "drink = {"
              , Text.unlines $ map convertToPythonDict drinks
              , "}"
              ]

main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String [Drink])
  case res of
    Left err -> putStrLn err
    Right drinks -> I.writeFile outputFile (generatePythonDict drinks)
