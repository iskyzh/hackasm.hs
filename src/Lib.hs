module Lib
    ( assemble
    ) where

import Instruction
import SymbolTable

import Data.Char (isDigit, isSpace)
import Prelude hiding (lookup)

import Data.Map.Strict (lookup)

assemble :: String -> String
assemble code = (unlines . map convert . assemble') parsedInstructions where
    instructions' = lines code
    filterSpace line = filter (not . isSpace) line
    instructions = filter (\row -> not (row == "")) (map filterSpace instructions')
    symbolTable = loadSymbol instructions
    parsedInstructions = loadInstruction instructions 

    assemble' :: [Maybe Instruction] -> [[Int]]
    assemble' (instruction:xs) = case instruction of
        Just instruction' -> (case instruction' of
            AInstruction label -> parseA label
            Instruction instruction -> instruction) : assemble' xs
        Nothing -> assemble' xs
    assemble' _ = []

    convert :: [Int] -> String
    convert (i:xs) = case i of
        0 -> '0' : convert xs
        1 -> '1' : convert xs
        _ -> convert xs
    convert _ = ""

    parseA :: String -> [Int]
    parseA label | all isDigit label = (bin . read) label
                 | otherwise = bin (case lookup label symbolTable of
                    Just n -> n
                    Nothing -> 0)
    
    bin :: Int -> [Int]
    bin n  = bin' n 16 where
        bin' _ 0 = []
        bin' n bit = (bin' (n `div` 2) (bit - 1)) ++ [n `mod` 2]