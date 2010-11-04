-- Evaluate the sum of a fibonacci and euler sum

module Main where
    
import System.Time
import Control.Parallel
    
    --Computes the fibonacci sum
    fib :: Int -> Int 
    fib 0 = 0 
    fib 1 = 1 
    fib n = fib (n-1) + fib (n-2)
    
    --Generates an euler Number
    euler :: Int -> Int
    euler n = length (filter (relprime n) (mkList n)) 
    
    --Evaluates the sum of euler numbers
    sumEuler :: Int -> Int
    sumEuler = sum . (map euler) . mkList
    
    --generates a list 1 .. n
    mkList :: Int -> [Int]
    mkList n = [1..n-1] 
    
    --Checks, if a number is prime
    relprime :: Int -> Int -> Bool
    relprime x y = gcd x y == 1 
        
    --Computes the sum of fib and euler - sequentiell
    sumFibEuler :: Int -> Int -> Int
    sumFibEuler a b = fib a + sumEuler b
    
    --Computes the sum of fib and euler - parallel
    parSumFibEuler :: Int -> Int -> Int 
    parSumFibEuler a b = par f ( e + f )
                        where
                            f = fib a 
                            e = sumEuler b
    --Computes the difference between 2 ClockTime Elements 
    secDiff :: ClockTime -> ClockTime -> Float 
    secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
    
    --r1 evaluates the function parSumFibEuler with the parameters 38 5300
    r1 :: Int 
    r1 = parSumFibEuler 38 5300
    
    main :: IO () 
    main = do 
            t0 <- getClockTime 
            pseq r1 (return ())
            t1 <- getClockTime 
            
            putStrLn ("sum: " ++ show r1) 
            putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds")
            
            
            