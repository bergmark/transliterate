module Translit.Fay where

import           Fay.FFI
import           Fay.Text (Text)
import           Prelude

fromCharCode :: Int -> Char
fromCharCode = ffi "String.fromCharCode(%1)"

charCodeAt :: Int -> Text -> Int
charCodeAt = ffi "%2.charCodeAt(%1)"

replace :: Text -> Text -> Text -> Text
replace = ffi "%3.replace(%1,%2,'g')"

split :: Text -> Text -> [Text]
split = ffi "%2.split(%1)"

matchI :: Text -> Text -> Maybe [Text]
matchI reg txt = fromNullable ((ffi "%2.match(%1,'i')" :: Text -> Text -> Nullable [Text]) reg txt)

join :: Text -> [Text] -> Text
join = ffi "%2.join(%1)"

fromNullable :: Nullable a -> Maybe a
fromNullable Null         = Nothing
fromNullable (Nullable a) = Just a
