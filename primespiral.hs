import Parsing
import Spiral 
import Primes
import TerminalUtilities

loop :: IO()
loop = do
    putStrLn ""
    str <- getLine
    cls
    tokens <- return $tokenize str
    command <- return $head tokens 
    case command of 
        "quit" -> quit (tail tokens)
        "prime" -> performGetprime (tail tokens)
        "spiral" -> performSpiral (tail tokens)
        "directions" -> performDirections (tail tokens)
        
        
--actions
quit :: [String] -> IO()
quit xs = return ()

performGetprime :: [String] -> IO()
performGetprime (a:xs) = do
    cls
    i <- return (read a)
    putStr $"The "++(show i)++"th prime number is "++ (show (getprime i))++"."
    loop 
    
performSpiral :: [String] -> IO()
performSpiral (a:b:xs) = do
    i <- return (read a)
    j <- return (read b) 
    showSpiral (makeSpiral i j)
    loop
    
performDirections :: [String] -> IO()
performDirections _ = do
    showDirections 1000 directions  
    loop 