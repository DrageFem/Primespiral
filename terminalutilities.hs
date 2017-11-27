module TerminalUtilities where 

import Types

writelines ::  Int -> Pos -> [String] -> IO()
writelines _ _ [] = return () 
writelines i (x,y) (str:xs) = do
    writeat (x,y) str 
    writelines (i+1) (x,y+1) xs 

cls :: IO()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs
                  
goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

wait :: Int-> IO()
wait 0 = return ()
wait n = wait (n-1)

seqn :: [IO()] -> IO()
seqn (x:xs) = do 
    x 
    if null xs 
        then return () 
        else seqn xs 