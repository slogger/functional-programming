module Main where

import SimpleQueue
import TwoListQueue
import BalancedTwoListQueue
import Queue
import System.CPUTime

extract q
    | empty q = ""
    | otherwise = extract (pop q)


test v = do
    start <- getCPUTime
    let q = foldr (\x q -> push x q) v [1..10000]
    putStrLn $ extract q
    end <- getCPUTime
    print $ (end - start) `div` 1000000000

main = do
    test (SimpleQueue [])
    test (TwoListQueue [] [])
    test (BalancedTwoListQueue [] [] 0 0)
