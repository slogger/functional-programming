module Main where

import qualified Data.Map.Strict as Map
import System.IO
import Data.List


readDict :: FilePath -> IO [String]
readDict fileName = do
    h <- openFile fileName ReadMode
    hSetEncoding h utf8
    s <- hGetContents h
    return $ lines s


diff ::    String -> String -> Bool
diff "" "" = False
diff a b
    | head a  == head b = (tail a) `diff` (tail b)
    | (head a  /= head b) && (tail a == tail b) = True
    | otherwise = False


search :: [String] -> String -> [String] -> Map.Map String String
    -> Map.Map String String
search list end dict map
    | first == end = map
    | otherwise = search list' end dict' map'
    where
        first = head list
        words = filter(\x -> diff first x) dict
        dict' = dict \\ words
        list' = (tail list) ++ words
        map' = foldl (\m w -> Map.insert w first m) map words


write :: Map.Map String String -> String -> String -> IO()
write path word start
    | word == start = return()
    | otherwise = do
        let next = path Map.! word
        putStrLn next
        write path next start

main = do
    let start = "муха"
    let finish = "слон"
    dict <- readDict "vocabulary.txt"
    putStrLn $ finish
    let path = search [start] finish dict Map.empty
    write path finish start
