{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson 
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.IO as I hiding (putStrLn)
import GHC.Generics

jsonFile :: FilePath
jsonFile = "DrinkRecipe.json"

outputFile :: FilePath
outputFile = "drink.py"

data DrinkAttribute = Flavor
                    | Kind
                    | Trait
                    | Price
                    | Recipe
                    deriving (Bounded, Enum, Show)

data DrinkFlavor = Bitter
                 | Bubbly
                 | Sour
                 | Spicy
                 | Sweet
                 deriving (Bounded, Enum, Show)

data DrinkKind = Classic
               | Classy
               | Girly
               | Manly
               | Promo
               deriving (Bounded, Enum, Show)

data DrinkTrait = Bland
                | Burning
                | Happy
                | Sobering
                | Soft
                | Strong
                | Vintage
                deriving (Bounded, Enum, Show)

data Ingredient = Adelhyde
                | BronsonExtract
                | PowderedDelta
                | Flanergide
                | Karmotrine
                deriving (Bounded, Enum, Show)

data RecipeAction = AddIce
                  | Age
                  | Wait
                  deriving (Bounded, Enum, Show)

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
              , wait :: Bool
              } deriving (Generic, Show)

instance FromJSON Drink
instance FromJSON DrinkRecipe

drinkAttributeConstants :: [DrinkAttribute]
drinkAttributeConstants = [minBound..maxBound]

drinkFlavorConstants :: [DrinkFlavor]
drinkFlavorConstants = [minBound..maxBound]

drinkKindConstants :: [DrinkKind]
drinkKindConstants = [minBound..maxBound]

drinkTraitConstants :: [DrinkTrait]
drinkTraitConstants = [minBound..maxBound]

ingredientConstants :: [Ingredient]
ingredientConstants = [minBound..maxBound]

recipeActionConstants :: [RecipeAction]
recipeActionConstants = [minBound..maxBound]

getRawJSON :: FilePath -> IO B.ByteString
getRawJSON = B.readFile

toText :: Show a => a -> Text
toText = Text.pack . show

constantsFromDrink :: [Text]
constantsFromDrink = map toText drinkAttributeConstants

constantsFromRecipe :: [Text]
constantsFromRecipe =
    concat [ map toText ingredientConstants
           , map toText recipeActionConstants
           , map toText drinkFlavorConstants
           , map toText drinkKindConstants
           , map toText drinkTraitConstants
           ]

convertConstantToSymbol :: Text -> Text
convertConstantToSymbol constant =
  case Text.uncons constant of
    Just (' ', remainder) -> Text.cons '_' $ convertConstantToSymbol remainder
    Just (letter, remainder) ->
        case Text.uncons remainder of
          Just (nextLetter, nextRemainder)
            | Char.isUpper nextLetter ->
                Text.cons capitalizedLetter
                . Text.cons '_'
                $ convertConstantToSymbol nextConstant
            | otherwise ->
                Text.cons capitalizedLetter
                $ convertConstantToSymbol remainder
            where nextConstant = Text.cons nextLetter nextRemainder
          Nothing -> Text.singleton capitalizedLetter
      where capitalizedLetter = Char.toUpper letter
    Nothing -> "NONE"

initializeConstant :: Text -> Text
initializeConstant constant = Text.concat [symbol, " = '", symbol, "'"]
  where symbol = convertConstantToSymbol constant

convertConstantsToInitializations :: [Text] -> [Text]
convertConstantsToInitializations = map initializeConstant

convertNameToSymbol :: Text -> Text
convertNameToSymbol drinkName =
  case Text.uncons drinkName of
    Just (' ', remainder) -> Text.cons '_' $ convertNameToSymbol remainder
    Just (letter, remainder) -> Text.cons (Char.toUpper letter) $ convertNameToSymbol remainder
    Nothing -> Text.empty

initializeName :: Text -> Text
initializeName drinkName = Text.concat [symbol, " = '", symbol, "'"]
    where symbol = convertNameToSymbol drinkName

convertNamesToInitializations :: [Text] -> [Text]
convertNamesToInitializations = map initializeName

initializeConstants :: [Drink] -> [Text]
initializeConstants drinks = convertConstantsToInitializations constants ++ convertNamesToInitializations drinkNames
  where
    drinkNames = map name drinks
    constants = constantsFromDrink
             ++ constantsFromRecipe
             ++ [""]

boolToText :: Bool -> Text
boolToText True = "True"
boolToText False = "False"

ingredientFromDrinkRecipe :: Ingredient -> DrinkRecipe -> Int
ingredientFromDrinkRecipe Adelhyde = adelhyde
ingredientFromDrinkRecipe BronsonExtract = bronsonExtract
ingredientFromDrinkRecipe PowderedDelta = powderedDelta
ingredientFromDrinkRecipe Flanergide = flanergide
ingredientFromDrinkRecipe Karmotrine = karmotrine

recipeActionFromDrinkRecipe :: RecipeAction -> DrinkRecipe -> Bool
recipeActionFromDrinkRecipe AddIce = addIce
recipeActionFromDrinkRecipe Age = age
recipeActionFromDrinkRecipe Wait = wait

convertDrinkRecipeToPythonDict :: DrinkRecipe -> Text
convertDrinkRecipeToPythonDict recipe =
    Text.concat
      [ "{"
      , Text.concat $ map formatIngredientKeyValuePair ingredientConstants
      , Text.concat $ map formatRecipeActionKeyValuePair recipeActionConstants
      , "\n    }"
      ]
  where
    colon = ": "
    comma = ", "
    indent = "\n      "
    formatIngredientKeyValuePair ingredient =
      Text.concat
        [ indent
        , convertConstantToSymbol $ toText ingredient
        , colon
        , toText $ ingredientFromDrinkRecipe ingredient recipe
        , comma
        ]
    formatRecipeActionKeyValuePair action =
      Text.concat
        [ indent
        , convertConstantToSymbol $ toText action
        , colon
        , boolToText $ recipeActionFromDrinkRecipe action recipe
        , comma
        ]

drinkAttributeSymbolFromDrink :: DrinkAttribute -> Drink -> Text
drinkAttributeSymbolFromDrink Flavor = convertConstantToSymbol . flavor
drinkAttributeSymbolFromDrink Kind = convertConstantToSymbol . kind
drinkAttributeSymbolFromDrink Trait = convertConstantToSymbol . trait
drinkAttributeSymbolFromDrink Price = toText . price
drinkAttributeSymbolFromDrink Recipe = convertDrinkRecipeToPythonDict . recipe

convertToPythonDict :: Drink -> Text
convertToPythonDict drink =
    Text.concat
      [ "  "
      , convertConstantToSymbol $ name drink
      , ": {"
      , Text.concat $ map formatDrinkAttributeKeyValuePair drinkAttributeConstants
      , "\n  },"
      ]
  where
    colon = ": "
    comma = ", "
    indent = "\n    "
    formatDrinkAttributeKeyValuePair attribute =
      Text.concat
        [ indent
        , convertConstantToSymbol $ toText attribute
        , colon
        , drinkAttributeSymbolFromDrink attribute drink
        , comma
        ]

generatePythonDict :: [Drink] -> Text
generatePythonDict drinks =
  Text.concat [ Text.unlines $ initializeConstants drinks
              , "drink = {\n"
              , Text.unlines $ map convertToPythonDict drinks
              , "}"
              ]

main = do
  res <- (eitherDecode <$> getRawJSON jsonFile) :: IO (Either String [Drink])
  case res of
    Left err -> putStrLn err
    Right drinks -> I.writeFile outputFile (generatePythonDict drinks)
