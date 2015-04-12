import Control.Monad
import Control.Monad.State.Lazy
import Data.List (partition, find)

type Word = String

is_edge :: Word -> Word -> Bool
is_edge w1 w2 = length l == 1
  where l = filter (not) $
            zipWith (==) w1 w2

type Graph a = ([a], a -> a -> Bool)

type Mark a = [(a, [a])]
type Path a = [a]

path :: (Eq a) => a -> Mark a -> Maybe (Path a)
path target marked =
  (\(a,as) -> (a:as)) `liftM` find (\x -> fst x == target) marked

mark :: Mark a -> Graph a -> (Mark a, Graph a)
mark marked graph = (concat markeds, graph')
  where
    (markeds, graph') = runState m graph
    m = forM marked $ \(a,as) -> do
      g <- get
      let (click, g') = removeClick g a
      case null click of
        True  -> return []
        False -> do
          put g'
          return $ map (\x -> (x, (a:as))) click

removeClick :: Graph a -> a -> ([a], Graph a)
removeClick (vertexes, predicate) vertex = (p1, (p2, predicate))
  where (p1,p2) = partition (predicate vertex) vertexes


findPath :: (Eq a) => Graph a -> a -> a -> Maybe (Path a)
findPath graph start target = findPath' graph [(start, [])]
  where
    findPath' graph marked =
      case null marked of
        True  -> Nothing
        False -> case target `path` marked of
          Just p  -> Just p
          Nothing -> let (marked', graph') = mark marked graph
                     in findPath' graph' marked'

main = do
  let pairs = [("муха", "слон")]

  words <- lines `liftM` (readFile "vocabulary.txt")
  let graph = (words, is_edge)
  forM_ pairs $ \(w1, w2) -> do
    case findPath graph w1 w2 of
      Nothing -> appendFile "out.txt" $ "Not words: " ++ w1 ++ " - " ++ w2 ++ "\n"
      Just ps -> appendFile "out.txt" $
                 (concatMap (++",") ps) ++ "\n"
