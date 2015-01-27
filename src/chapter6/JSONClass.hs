module JSONClass
    (
      JSON(..)
    , JValue(..)
    , JAry(..)
    , JObj(..)
    ) where

import Control.Arrow (second)

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

jary :: [a] -> JAry a
jary = JAry

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "Not a JSON boolean"
