module Application
    where

application :: Monad m => (m String) -> (String -> m ()) -> m ()
application inputFunction outputFunction = do
    s <- inputFunction
    outputFunction (s <> s)
