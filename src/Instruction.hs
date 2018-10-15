module Instruction
    ( loadInstruction,
      Instruction (..)
    ) where

        import Parse

        data Instruction = AInstruction String | Instruction [Int] deriving Show

        loadInstruction :: [String] -> [Maybe Instruction]
        
        loadInstruction (instruction:xs) = (parse instruction) : loadInstruction xs where
            parse :: String -> Maybe Instruction
            parse ('@':a) = Just (AInstruction a)
            parse ('(':label) = Nothing
            parse dInstruction = Just (Instruction ([1, 1, 1] ++ parseToken dInstruction parseFunc parseOper)) where
                parseFunc = [parseDest, parseOperation, parseJump]
                parseOper = [(== '='), (== ';'), (\x -> False)]
                
                parseToken :: String -> [String -> [Int]] -> [Char -> Bool] -> [Int]
                parseToken instruction (func:funcs) (oper:opers) = let (ins, rest) = break oper instruction in 
                    (func ins) ++ case rest of
                        "" -> parseToken "" funcs opers
                        _ -> parseToken (tail rest) funcs opers
                parseToken _ _ _ = []
        loadInstruction _ = []