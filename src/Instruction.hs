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
            parse dInstruction = Just (Instruction ([1, 1, 1] ++ parseToken dInstruction))
            
            parseToken :: String -> [Int]
            parseToken instruction = let (dest, oper, jump) = breakToken in
                parseOperation oper ++ parseDest dest ++ parseJump jump where
                    breakToken = let (rest, jump) = break (==';') instruction
                                     (dest, oper) = break (=='=') rest in 
                                        case oper of
                                            "" -> ("", dest, tryTail jump)
                                            _ -> (dest, tryTail oper, tryTail jump) 
                                     where
                                        tryTail (x:xs) = xs
                                        tryTail _ = ""
        loadInstruction _ = []