module Stack where

import Control.Monad.State

data Stack a = Stack a (Stack a) | Empty deriving (Show)

type MonadStack a b = State (Stack a) b

push :: a -> MonadStack a ()
push x = do
  stack <- get
  put (Stack x stack)
  pure ()

pop :: MonadStack a (Maybe a)
pop = do
  stack <- get
  case stack of
    Stack x stack' -> do
      put stack'
      pure (Just x)
    Empty ->
      pure Nothing

top :: MonadStack a (Maybe a)
top = do
  stack <- get
  case stack of
    Stack x stack' -> do
      pure (Just x)
    Empty ->
      pure Nothing

empty :: MonadStack a Bool
empty =
  gets empty'
  where
    empty' Empty = True
    empty' (Stack _ _) = False

size :: MonadStack a Int
size =
  gets (size' 0)
  where
    size' n Empty = n
    size' n (Stack _ stack) = size' (n + 1) stack

-- >>> runState (push 1 >> push 2 >> push 3 >> pop) Empty
-- (Just 3,Stack 2 (Stack 1 Empty))
