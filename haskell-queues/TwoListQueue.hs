module TwoListQueue where
import Queue

data TwoListQueue a = TwoListQueue [a] [a] deriving Show

instance Queue TwoListQueue where
    create x = TwoListQueue [x] []
    push x (TwoListQueue left right) = TwoListQueue left (x:right)
    pop (TwoListQueue left right)
        | null t = TwoListQueue (reverse right) []
        | otherwise = TwoListQueue t right
        where
            t = tail left
    top (TwoListQueue left r) = head left
    empty (TwoListQueue left right) = null left
