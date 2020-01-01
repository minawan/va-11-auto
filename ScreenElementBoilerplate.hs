{-# LANGUAGE OverloadedStrings #-}

import Data.Csv (Header, decodeByName)
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Text.Printf (printf)

import ScreenElement

data Category =
  Category { button :: Either String Button
           , ingredient :: Either String Ingredient
           , other :: Either String Other
           } deriving (Show)

data Button =
  Button { addIce :: Either String Element
         , age :: Either String Element
         , leftSlot :: Either String Element
         , rightSlot :: Either String Element
         , reset :: Either String Element
         , mix :: Either String Element
         } deriving (Show)

data Ingredient =
  Ingredient { adelhyde :: Either String Element
             , bronsonExtract :: Either String Element
             , powderedDelta :: Either String Element
             , flanergide :: Either String Element
             , karmotrine :: Either String Element
             } deriving (Show)

data Other = Other { blender :: Either String Element } deriving (Show)

data Element = Element Int Int Int deriving (Show)

buttonLiteral :: String
buttonLiteral = "Button"

addIceLiteral :: String
addIceLiteral = "addIce"

ageLiteral :: String
ageLiteral = "age"

leftSlotLiteral :: String
leftSlotLiteral = "leftSlot"

rightSlotLiteral :: String
rightSlotLiteral = "rightSlot"

resetLiteral :: String
resetLiteral = "reset"

mixLiteral :: String
mixLiteral = "mix"

ingredientLiteral :: String
ingredientLiteral = "Ingredient"

adelhydeLiteral :: String
adelhydeLiteral = "adelhyde"

bronsonExtractLiteral :: String
bronsonExtractLiteral = "bronsonExtract"

powderedDeltaLiteral :: String
powderedDeltaLiteral = "powderedDelta"

flanergideLiteral :: String
flanergideLiteral = "flanergide"

karmotrineLiteral :: String
karmotrineLiteral = "karmotrine"

otherLiteral :: String
otherLiteral = "Other"

blenderLiteral :: String
blenderLiteral = "blender"

xCoordLiteral :: String
xCoordLiteral = "xCoord"

yCoordLiteral :: String
yCoordLiteral = "yCoord"

shortcutLiteral :: String
shortcutLiteral = "shortcut"

convertConstantToSymbol :: String -> String
convertConstantToSymbol "" = "NONE"
convertConstantToSymbol [letter] = [Char.toUpper letter]
convertConstantToSymbol (' ':remainder) =
    '_' : convertConstantToSymbol remainder
convertConstantToSymbol (letter:nextLetter:nextRemainder)
  | Char.isUpper nextLetter =
      capitalizedLetter : '_' : nextConstant
  | otherwise = capitalizedLetter : nextConstant
  where
    capitalizedLetter = Char.toUpper letter
    nextConstant = convertConstantToSymbol $ nextLetter : nextRemainder

initializeConstants :: String
initializeConstants =
    unlines $
      map (uncurry (printf "%s = '%s'") . \ constant ->
        (convertConstantToSymbol constant, constant)) literals
  where
    literals = [ buttonLiteral
               , addIceLiteral
               , ageLiteral
               , leftSlotLiteral
               , rightSlotLiteral
               , resetLiteral
               , mixLiteral
               , ingredientLiteral
               , adelhydeLiteral
               , bronsonExtractLiteral
               , powderedDeltaLiteral
               , flanergideLiteral
               , karmotrineLiteral
               , otherLiteral
               , blenderLiteral
               , xCoordLiteral
               , yCoordLiteral
               , shortcutLiteral
               ]

structurize :: [ScreenElement] -> Category
structurize xs = Category (getButton xs)
                      (getIngredient xs)
                      (getOther xs)

isButtonElement :: ScreenElement -> Bool
isButtonElement (ScreenElement _ category _ _ _) = category == "Button"

isIngredientElement :: ScreenElement -> Bool
isIngredientElement (ScreenElement _ category _ _ _) = category == "Ingredient"

isOtherElement :: ScreenElement -> Bool
isOtherElement (ScreenElement _ category _ _ _) = category == "Other"

getButton :: [ScreenElement] -> Either String Button
getButton [] = Left "ScreenElement not found in getButton."
getButton xs = case elements of
                 [] -> Left "Button not found."
                 buttonElements ->
                   Right $ Button
                     (getAddIce buttonElements)
                     (getAge buttonElements)
                     (getLeftSlot buttonElements)
                     (getRightSlot buttonElements)
                     (getReset buttonElements)
                     (getMix buttonElements)
  where elements = [x | x <- xs, isButtonElement x]

isAddIceElement :: ScreenElement -> Bool
isAddIceElement (ScreenElement name _ _ _ _) = name == "addIce"

isAgeElement :: ScreenElement -> Bool
isAgeElement (ScreenElement name _ _ _ _) = name == "age"

isLeftSlotElement :: ScreenElement -> Bool
isLeftSlotElement (ScreenElement name _ _ _ _) = name == "leftSlot"

isRightSlotElement :: ScreenElement -> Bool
isRightSlotElement (ScreenElement name _ _ _ _) = name == "rightSlot"

isResetElement :: ScreenElement -> Bool
isResetElement (ScreenElement name _ _ _ _) = name == "reset"

isMixElement :: ScreenElement -> Bool
isMixElement (ScreenElement name _ _ _ _) = name == "mix"

getElement :: (ScreenElement -> Bool) -> [ScreenElement] -> Either String Element
getElement pred xs =
  case [x | x <- xs, pred x] of
    [] -> Left "Empty list."
    [ScreenElement _ _ xCoord yCoord shortcut] ->
      Right $ Element xCoord yCoord shortcut
    _ -> Left "Not a singleton."

getAddIce :: [ScreenElement] -> Either String Element
getAddIce = getElement isAddIceElement

getAge :: [ScreenElement] -> Either String Element
getAge = getElement isAgeElement

getLeftSlot :: [ScreenElement] -> Either String Element
getLeftSlot = getElement isLeftSlotElement

getRightSlot :: [ScreenElement] -> Either String Element
getRightSlot = getElement isRightSlotElement

getReset :: [ScreenElement] -> Either String Element
getReset = getElement isResetElement

getMix :: [ScreenElement] -> Either String Element
getMix = getElement isMixElement

getIngredient :: [ScreenElement] -> Either String Ingredient
getIngredient [] = Left "ScreenElement not found."
getIngredient xs =
    case elements of
      [] -> Left "Ingredient not found."
      ingredientElements ->
        Right $ Ingredient
          (getAdelhyde ingredientElements)
          (getBronsonExtract ingredientElements)
          (getPowderedDelta ingredientElements)
          (getFlanergide ingredientElements)
          (getKarmotrine ingredientElements)
  where elements = [x | x <- xs, isIngredientElement x]

isAdelhydeElement :: ScreenElement -> Bool
isAdelhydeElement (ScreenElement name _ _ _ _) = name == "adelhyde"

isBronsonExtractElement :: ScreenElement -> Bool
isBronsonExtractElement (ScreenElement name _ _ _ _) = name == "bronsonExtract"

isPowderedDeltaElement :: ScreenElement -> Bool
isPowderedDeltaElement (ScreenElement name _ _ _ _) = name == "powderedDelta"

isFlanergideElement :: ScreenElement -> Bool
isFlanergideElement (ScreenElement name _ _ _ _) = name == "flanergide"

isKarmotrineElement :: ScreenElement -> Bool
isKarmotrineElement (ScreenElement name _ _ _ _) = name == "karmotrine"

getAdelhyde :: [ScreenElement] -> Either String Element
getAdelhyde = getElement isAdelhydeElement

getBronsonExtract :: [ScreenElement] -> Either String Element
getBronsonExtract = getElement isBronsonExtractElement

getPowderedDelta :: [ScreenElement] -> Either String Element
getPowderedDelta = getElement isPowderedDeltaElement

getFlanergide :: [ScreenElement] -> Either String Element
getFlanergide = getElement isFlanergideElement

getKarmotrine :: [ScreenElement] -> Either String Element
getKarmotrine = getElement isKarmotrineElement

getOther :: [ScreenElement] -> Either String Other
getOther [] = Left "ScreenElement not found in getOther."
getOther xs = case elements of
                [] -> Left "Other not found."
                otherElements -> Right $ Other (getBlender otherElements)
  where elements = [x | x <- xs, isOtherElement x]

isBlenderElement :: ScreenElement -> Bool
isBlenderElement (ScreenElement name _ _ _ _) = name == "blender"

getBlender :: [ScreenElement] -> Either String Element
getBlender = getElement isBlenderElement

toPythonDict :: Category -> String
toPythonDict (Category button ingredient other) =
    unlines [ "centroid = {"
            , buttonToPythonDict button
            , ingredientToPythonDict ingredient
            , otherToPythonDict other
            , "}"
            ]

buttonToPythonDict :: Either String Button -> String
buttonToPythonDict (Left err) = err
buttonToPythonDict (Right (Button addIce age leftSlot rightSlot reset mix)) =
    unlines [ printf "%s: {" $ convertConstantToSymbol buttonLiteral
            , addIceToPythonDict addIce
            , ageToPythonDict age
            , leftSlotToPythonDict leftSlot
            , rightSlotToPythonDict rightSlot
            , resetToPythonDict reset
            , mixToPythonDict mix
            , "},"
            ]

addIceToPythonDict :: Either String Element -> String
addIceToPythonDict = elementToPythonDict addIceLiteral

ageToPythonDict :: Either String Element -> String
ageToPythonDict = elementToPythonDict ageLiteral

leftSlotToPythonDict :: Either String Element -> String
leftSlotToPythonDict = elementToPythonDict leftSlotLiteral

rightSlotToPythonDict :: Either String Element -> String
rightSlotToPythonDict = elementToPythonDict rightSlotLiteral

resetToPythonDict :: Either String Element -> String
resetToPythonDict = elementToPythonDict resetLiteral

mixToPythonDict :: Either String Element -> String
mixToPythonDict = elementToPythonDict mixLiteral

ingredientToPythonDict :: Either String Ingredient -> String
ingredientToPythonDict (Left err) = err
ingredientToPythonDict (Right (Ingredient adelhyde bronsonExtract powderedDelta flanergide karmotrine)) =
    unlines [ printf "%s: {" $ convertConstantToSymbol ingredientLiteral
            , adelhydeToPythonLiteral adelhyde
            , bronsonExtractToPythonLiteral bronsonExtract
            , powderedDeltaToPythonLiteral powderedDelta
            , flanergideToPythonLiteral flanergide
            , karmotrineToPythonLiteral karmotrine
            , "},"
            ]

adelhydeToPythonLiteral :: Either String Element -> String
adelhydeToPythonLiteral = elementToPythonDict adelhydeLiteral

bronsonExtractToPythonLiteral :: Either String Element -> String
bronsonExtractToPythonLiteral = elementToPythonDict bronsonExtractLiteral

powderedDeltaToPythonLiteral :: Either String Element -> String
powderedDeltaToPythonLiteral = elementToPythonDict powderedDeltaLiteral

flanergideToPythonLiteral :: Either String Element -> String
flanergideToPythonLiteral = elementToPythonDict flanergideLiteral

karmotrineToPythonLiteral :: Either String Element -> String
karmotrineToPythonLiteral = elementToPythonDict karmotrineLiteral

otherToPythonDict :: Either String Other -> String
otherToPythonDict (Left err) = err
otherToPythonDict (Right (Other blender)) =
    unlines [ printf "%s: {" $ convertConstantToSymbol otherLiteral
            , blenderToPythonDict blender
            , "},"
            ]

blenderToPythonDict :: Either String Element -> String
blenderToPythonDict = elementToPythonDict blenderLiteral

elementToPythonDict :: String -> Either String Element -> String
elementToPythonDict constant (Left err) = printf "%s: %s" constant err
elementToPythonDict constant (Right (Element xCoord yCoord shortcut)) =
    unlines [ printf "%s: {" $ convertConstantToSymbol constant
            , printf "%s: %d," (convertConstantToSymbol xCoordLiteral) xCoord
            , printf "%s: %d," (convertConstantToSymbol yCoordLiteral) yCoord
            , printf "%s: %d," (convertConstantToSymbol shortcutLiteral) shortcut
            , "},"
            ]

main :: IO ()
main = do
  csvData <- B.readFile "ScreenElement.csv"
  let decodedCsvData = decodeByName csvData :: Either String (Header, Vector ScreenElement)
  case decodedCsvData of
    Left err -> putStrLn err
    Right (_, val) -> do
      putStrLn initializeConstants
      putStrLn . toPythonDict . structurize $ Vector.toList val

