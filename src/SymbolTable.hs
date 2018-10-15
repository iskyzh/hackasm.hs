module SymbolTable
    ( loadSymbol
    ) where
        import Prelude hiding (lookup)
        import Data.Char (isDigit)
        import Data.Map.Strict (Map, fromList, insert, lookup)
        loadSymbol :: [String] -> Map String Int
        loadSymbol instructions = loadVar' instructions (fromList (loadSymbol' instructions 0)) 16

        loadSymbol' :: [String] -> Int -> [(String, Int)]
        loadSymbol' (('(':label):xs) line = ((reverse . drop 1 . reverse) label, line) : (loadSymbol' xs line)
        loadSymbol' (_:xs) line = loadSymbol' xs (line + 1)
        loadSymbol' _ _ = [("SP", 0), ("LCL", 1), ("ARG", 2), ("THIS", 3), ("THAT", 4),
                           ("R0", 0), ("R1", 1), ("R2", 2), ("R3", 3), ("R4", 4), ("R5", 5), ("R6", 6),
                           ("R7", 7), ("R8", 8), ("R9", 9), ("R10", 10), ("R11", 11), ("R12", 12), ("R13", 13),
                           ("R14", 14), ("R15", 15), ("SCREEN", 16384), ("KBD", 24576)]
        loadVar' :: [String] -> Map String Int -> Int -> Map String Int
        loadVar' (('@':label):xs) symbols id | all isDigit label = loadVar' xs symbols id
                                             | otherwise = case lookup label symbols of
                                                    Nothing -> loadVar' xs (insert label id symbols) (id + 1)
                                                    _ -> loadVar' xs symbols id
        loadVar' (ins:xs) symbols id = loadVar' xs symbols id
        loadVar' _ symbols _ = symbols