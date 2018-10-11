module Instruction
    ( loadInstruction
    ) where

import Data.Map (Map)
import Prase

data Instruction = AInstruction Integer | ALabel String | DInstruction String
data SymbolTable = SymbolTable Map String Integer

loadInstruction :: [String] -> [Maybe Instruction]

loadInstruction (instruction:xs) = parse instruction
    where
            parse :: String -> Integer -> Meybe Instruction
            parse ('@':addr) line = Just parseA addr ++ loadInstruction xs
            parse ('(':label:')') = Nothing ++ loadInstruction xs line
            parse (dest:'=':operation) = Just parseD dest operation "" ++ loadInstruction xs
            parse (dest:';':jump) = Just parseD dest "" jump ++ loadInstruction xs
            parse _ = Nothing

            parseA :: String -> Instruction
            parseA addr
                        | all isDigit addr = AInstruction read addr :: Integer
                        | otherwise = ALabel addr
            
            parseD :: String -> String -> String -> Instruction
            parseD dest operation jump = parseDest dest ++ parseOperation operation ++ parseJump jump