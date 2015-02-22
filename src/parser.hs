{-# LANGUAGE OverloadedStrings #-}
-- AttoParsec module is intended for parsing text
-- using 8-bit character set, e.g. ASCII / ISO-8859-15.
import Data.Word
import Data.Attoparsec.Char8

data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP :: Parser IP
parseIP = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    char '.'
    d3 <- decimal
    char '.'
    d4 <- decimal
    return $ IP d1 d2 d3 d4

main :: IO()
main = print $ parseOnly parseIP "131.45.68.123"
    

