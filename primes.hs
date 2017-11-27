module Primes where  

getprime :: Int-> Int
getprime n = primes !! n 

isprime :: Int -> Bool
isprime 2 = True
isprime p = null [i | i<-[2..weedroot p], p `mod` i == 0]

weedroot :: Int -> Int 
weedroot a = head [b | b<- iterate (*2) 1, b*b>a ]
    
primes :: [Int]
primes = filter isprime [2..]

nextprime :: Int-> Int
nextprime p = head (filter isprime [p+1..])