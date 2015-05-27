module SimpleQueue where

import Queue

data SimpleQueue a = SimpleQueue [a] deriving Show
-- realize
instance Queue SimpleQueue where
    create x = SimpleQueue [x]
    push x (SimpleQueue queue) = SimpleQueue (queue ++ [x])
    pop (SimpleQueue queue) = SimpleQueue (tail queue)
    top (SimpleQueue queue) = head queue
    empty (SimpleQueue queue) = null queue
