module Spiral where 

import Primes
import TerminalUtilities
import Types

--Position 0 is the middle of the spiral. Position (i+1) is
--the next position in the spiral. 
--
--Steps is the number of positions travelled forward from a
--given direction. 
--
--Checkpoint is the newest direction entered in the spiral.
--
--A lapse is completed by going from the first position in south
--going through  east north west and to the first position in south
--respectively. 

data Direction = South | East | North | West deriving (Show, Eq)  
data Spiral = Board [(Int,Pos,Int, Direction)] Int

emptySpiral :: Int->  Spiral
emptySpiral n= Board [(2,translate (div n 2,div n 2),1,East)] n

makeSpiral :: Int-> Int-> Spiral 
makeSpiral i n = generate i (emptySpiral n)

showSpiral :: Spiral -> IO()
showSpiral (Board [] n) = return () 
showSpiral (Board (x:xs) n) = do
    showPrime x n 
    showSpiral (Board xs n)
    
showDirections :: Int ->[Direction] -> IO()
showDirections 0 xs = return ()
showDirections n [] = return ()
showDirections n (d:dirs) = do
    putStr (show d++ " ")
    showDirections (n-1) dirs 
    
showPrime :: (Int,Pos,Int,Direction)-> Int -> IO()
showPrime (p,pos,_,_) n= writeat (translate pos) (show p)

translate :: Pos -> Pos
translate (x,y) = (x*4,y*2)

next :: Spiral -> Spiral
next (Board ((p,pos,i,dir):xs) n) = Board ((nextprime p,nextpos pos i dir,i+1, nextdir i):(p, pos, i, dir):xs) n
 
nextpos :: Pos -> Int-> Direction-> Pos
nextpos pos i dir = move pos $ directions!! i
 
directions :: [Direction]
directions = data2directions directionsData East  
 
data2directions :: [Int] -> Direction -> [Direction]
data2directions (x:xs) dir = if x >0
    then dir:data2directions ((x-1):xs) dir 
    else (changedir dir):data2directions xs (changedir dir)
                           

 
directionsData :: [Int]
directionsData = correction 2 $map (\l -> l-2) $concat $map g $zip4 south east north west
    where g (a,b,c,d) = [a,b,c,d]

correction :: Int-> [Int]-> [Int]
correction a (x:xs) = (x+a):xs
    
generate :: Int -> Spiral -> Spiral
generate 0 s = s
generate n s = next (generate (n-1) s)

move :: Pos -> Direction -> Pos
move (x,y) East = (x+1,y)
move (x,y) North = (x,y-1)
move (x,y) West = (x-1,y)
move (x,y) South = (x,y+1)
    
--Analyzation of spiral 
south = [3+4*k| k<-[0..]]
east= [3+4*k| k<-[0..]]
north = [5+4*k| k<-[0..]]
west = [5+4*k| k<-[0..]]

nextdir :: Int -> Direction
nextdir i = directions !! i

changedir North = West
changedir West = South
changedir South = East
changedir East = North

zip4:: [Int] ->[Int] ->[Int] ->[Int] ->[(Int,Int,Int,Int)]
zip4 (b:bs) (c:cs) (d:ds) (e:es) = (b,c,d,e):zip4 bs cs ds es


sumlapse (a,b,c,d) = a+b+c+d-4

position lapses finaldir steps = sizeFinishedlapses lapses
    + sizeCheckpoint lapses finaldir
    + steps


sizeCheckpoint :: Int-> Direction -> Int
sizeCheckpoint lapses dir | dir ==South = 0
    | dir == East = (south !! lapses)-1
    | dir == North = sizeCheckpoint lapses East + (east !! lapses)-1
    | dir == West = sizeCheckpoint lapses North + (north !! lapses) -1


sizeFinishedlapses n = 
    1 + (sum $map sumlapse 
    $take n $zip4 south east north west)