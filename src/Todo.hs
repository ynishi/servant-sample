module Todo
  ( Todo(..)
  , Todos
  ) where

data Todo = Todo
  { todoId :: Int
  , title  :: String
  } deriving (Eq, Show)

type Todos = [Todo]
