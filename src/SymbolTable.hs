module SymbolTable
    ( loadSymbol
    ) where

        import Data.Map.Strict (Map, fromList)
        loadSymbol :: [String] -> Map String Int
        loadSymbol instructions = fromList (loadSymbol' instructions 0) 

        loadSymbol' :: [String] -> Int -> [(String, Int)]
        loadSymbol' (('(':label):xs) line = ((reverse . drop 1 . reverse) label, line) : (loadSymbol' xs line)
        loadSymbol' (_:xs) line = loadSymbol' xs (line + 1)
        loadSymbol' _ _ = []
        