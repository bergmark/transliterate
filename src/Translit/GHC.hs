{-# LANGUAGE OverloadedStrings #-}

module Translit.GHC where

import           Data.Char
import qualified Data.List       as L
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Regex.PCRE

fromCharCode :: Int -> Char
fromCharCode = chr

charCodeAt :: Int -> Text -> Int
charCodeAt i t = ord $ T.index t i

replace :: Text -> Text -> Text -> Text
replace = T.replace

split :: Text -> Text -> [Text]
split "" s = map T.singleton (T.unpack s)
split t  s = T.splitOn t s

matchI :: Text -> Text -> Maybe [Text]
matchI reg txt = case T.unpack txt =~ T.unpack reg :: (String,String,String,[String]) of
  (_,_,_,[a,b,c]) -> Just [undefined, T.pack a, T.pack b, T.pack c]
  _               -> Nothing

join :: Text -> [Text] -> Text
join = T.intercalate

ifThenElse :: Bool -> a -> a -> a
ifThenElse a b c = if a then b else c

find :: (a -> Bool) -> [a] -> Maybe a
find = L.find
