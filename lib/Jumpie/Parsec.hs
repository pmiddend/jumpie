{-# LANGUAGE FlexibleContexts #-}
module Jumpie.Parsec(
  int,
  safeParseFromFile
  ) where

import Text.Parsec.Prim(Stream,ParsecT)
import Text.Parsec.String(parseFromFile,Parser)
import Text.Parsec(many1,digit)
import Text.Read(read)
import Data.List((++))
import Text.Show(show)
import Data.String(String)
import Control.Applicative((<$>))
import Prelude(Char,error)
import Data.Int(Int)
import System.IO(IO)
import Data.Either(Either(Left,Right))
import Data.Function(($))
import Control.Monad(return)

int :: Stream s m Char => ParsecT s u m Int
int = rd <$> many1 digit
  where rd = read :: String -> Int

safeParseFromFile :: Parser a -> String -> IO a
safeParseFromFile p f = do
  result <- parseFromFile p f
  return $ case result of
    Left e -> error $ "Parse error: " ++ (show e)
    Right r -> r
