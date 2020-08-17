module Brack.Utils.Common where

import Control.Monad.Trans.Class
import Control.Monad.State.Strict

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 1 |$>
(|$>) :: Functor f => f a -> (a -> b) -> f b
x |$> f = f <$> x

repeatUntilIdempotent :: Eq t => (t -> t) -> t -> t
repeatUntilIdempotent f x = if x == x' then x else repeatUntilIdempotent f x'
    where x' = f x

-- | wrapper type that changes the behavior of equality.
-- All values of this type are equal (for Eq and Ord).
-- Useful for tagging where expr equality is tag-independent
newtype AllSame a = AllSame{ unSame :: a } deriving(Show)

instance Eq (AllSame a) where
  _ == _ = True

instance Ord (AllSame a) where
  _ `compare` _ = EQ

class Tagged f where
  getTag :: f a -> a

type StateExceptT s e v = StateT s (Either (e,s)) v

throw :: e -> StateExceptT s e v
throw err = do
    s <- get
    lift (Left (err, s))
