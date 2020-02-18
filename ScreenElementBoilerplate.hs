{-# LANGUAGE OverloadedStrings #-}

import Data.Csv (Header, decodeByName)
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Text.Printf (printf)

import ScreenElement

inputFile :: FilePath
inputFile = "ScreenElement.csv"

outputFile :: FilePath
outputFile = "centroid.py"

categoryLiteral :: String
categoryLiteral = "Category"

buttonLiteral :: String
buttonLiteral = "Button"

addIceLiteral :: String
addIceLiteral = "AddIce"

ageLiteral :: String
ageLiteral = "Age"

leftSlotLiteral :: String
leftSlotLiteral = "LeftSlot"

rightSlotLiteral :: String
rightSlotLiteral = "RightSlot"

resetLiteral :: String
resetLiteral = "Reset"

mixLiteral :: String
mixLiteral = "Mix"

ingredientLiteral :: String
ingredientLiteral = "Ingredient"

adelhydeLiteral :: String
adelhydeLiteral = "Adelhyde"

bronsonExtractLiteral :: String
bronsonExtractLiteral = "BronsonExtract"

powderedDeltaLiteral :: String
powderedDeltaLiteral = "PowderedDelta"

flanergideLiteral :: String
flanergideLiteral = "Flanergide"

karmotrineLiteral :: String
karmotrineLiteral = "Karmotrine"

otherLiteral :: String
otherLiteral = "Other"

blenderLiteral :: String
blenderLiteral = "Blender"

xCoordLiteral :: String
xCoordLiteral = "XCoord"

yCoordLiteral :: String
yCoordLiteral = "YCoord"

shortcutLiteral :: String
shortcutLiteral = "Shortcut"

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

getLiterals :: [ScreenElement] -> [String]
getLiterals [] = []
getLiterals ((ScreenElement name category _ _ _):elements) =
    name : category : getLiterals elements

initializeConstants :: [ScreenElement] -> String
initializeConstants elements =
    unlines $
      map (uncurry (printf "%s = '%s'") . \ constant ->
        (convertConstantToSymbol constant, constant)) literals
  where
    literals = List.sort $
                 [ categoryLiteral
                 , xCoordLiteral
                 , yCoordLiteral
                 , shortcutLiteral
                 ] ++ (Set.toList . Set.fromList $ getLiterals elements)

toPythonDict :: [ScreenElement] -> String
toPythonDict elements =
    unlines [ "centroid = {"
            , unlines $ map screenElementToPythonDict elements
            , "}"
            ]

screenElementToPythonDict :: ScreenElement -> String
screenElementToPythonDict (ScreenElement name category xCoord yCoord shortcut) =
    unlines [ printf "%s: {" (convertConstantToSymbol name)
            , printf "%s: %s," (convertConstantToSymbol categoryLiteral) (convertConstantToSymbol category)
            , printf "%s: %d," (convertConstantToSymbol xCoordLiteral) xCoord
            , printf "%s: %d," (convertConstantToSymbol yCoordLiteral) yCoord
            , printf "%s: %d," (convertConstantToSymbol shortcutLiteral) shortcut
            , "},"
            ]

main :: IO ()
main = do
  csvData <- B.readFile inputFile
  let decodedCsvData = decodeByName csvData :: Either String (Header, Vector ScreenElement)
  case decodedCsvData of
    Left err -> putStrLn err
    Right (_, val) -> do
      writeFile outputFile . initializeConstants $ Vector.toList val
      appendFile outputFile . toPythonDict $ Vector.toList val

