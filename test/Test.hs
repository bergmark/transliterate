{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Test where

import           Fay.Text
import           Prelude
import           Translit.Hangeul

main = do
  p  $ toHangeul "an nyeong"
  pc $ toHangeulBlock "han"
  pc $ jamoFromChar "h"
  p  $ toHangeul "han geul"
  p  $ toHangeul "han  geul"
  pm $ fromHangeulBlock "한"
  p  $ fromHangeul "한글"
  p  $ fromHangeul "한 글"
  where
    p = putStrLn . unpack
    pm Nothing = putStrLn (unpack "Nothing")
    pm (Just x) = putStrLn (unpack x)
    pc Nothing = putStrLn (unpack "Nothing")
    pc (Just x) = putStrLn . (:[]) $ x
