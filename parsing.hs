module Parsing where 

import Data.Char

tokenize :: String -> [String]
tokenize [] = [] 
tokenize (x:xs) | x== ' ' = tokenize xs
                 | isNumber x = parseWhile isNumber
                 | isAlpha x = parseWhile isAlpha
    where parseWhile g= (x:takeWhile g xs):tokenize (dropWhile g xs)
    
