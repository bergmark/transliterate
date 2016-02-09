{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module FayTestWeb where

import           Fay.Text
import           JQuery
import           Prelude
import           Translit.Hangeul

main :: Fay ()
main = ready $ do
  inp <- select "#inp"
  out <- select "#out"

  (`keyup` inp) $ \_ -> do
    t <- getVal inp
    void $ setVal (toHangeul t) out
  (`keyup` out) $ \_ -> do
    b <- getVal out
    void $ setVal (fromHangeul b) inp
