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
    setVal (toHangeul t) out
    return ()
  (`keyup` out) $ \_ -> do
    b <- getVal out
    setVal (fromHangeul b) inp
    return ()
