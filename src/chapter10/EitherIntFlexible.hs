{-# LANGUAGE FlexibleInstances #-}

import Data.Either hiding ( Functor )

instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)
