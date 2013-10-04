{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Translit.Hangeul where

import           Fay.Text
import qualified Prelude      as L
#ifdef FAY
import           Prelude      hiding (intercalate, intersperse)
import           Translit.Fay
#else
import           Prelude
import           Translit.GHC
#endif

-- | Transliterate a string of hangeul to the latin alphabet.
fromHangeul :: Text -> Text
fromHangeul =
    replace "   " "  "
  . join " "
  . fromMaybes
  . L.map (\v -> if v == " " then Just v else fromHangeulBlock v)
  . split ""

-- | Transliterate a latin script into hangeul.
toHangeul :: Text -> Text
toHangeul =
    join ""
  . mapMaybe (\c -> if c == "" then Just " " else (`cons` "") <$> toHangeulBlock c)
  . split " "

-- | Turn a single hangul character (block) into latin script.
fromHangeulBlock :: Text -> Maybe Text
fromHangeulBlock syllable =
  let code         = charCodeAt 0 syllable - 44032
      initialIndex = code `div` 588
      code'        = code - initialIndex * 588
      vowelIndex   = code' `div` 28
      finalIndex   = code' - vowelIndex * 28
  in     aux initial initialIndex
     <+> aux vowel   vowelIndex
     <+> aux final   finalIndex
  where
    aux :: [(Text, Int, Text)] -> Int -> Maybe Text
    aux coll index = (\c -> if c == "_" then "" else c) <$> lookI index coll

-- | Convert a latin sequence into a jamo.
jamoFromChar :: Text -> Maybe Char
jamoFromChar c = fromCharCode <$> lookT c single

-- | Convert a latin sequence to a hangeul character.
toHangeulBlock :: Text -> Maybe Char
toHangeulBlock syllable =
  case matchI "^([bcdghjklmnprst]*)([aeiouyw]*)([bcdghjklmnprst]*)$" syllable of
    Nothing           -> Nothing
    Just [_,"","",""] -> Nothing
    Just [_,a ,b ,c ] ->
      Just . fromCharCode $
        blockCode (aux a initial)
                  (aux b vowel  )
                  (aux c final  )
    _                 -> Nothing
  where
    aux x = fromMaybe 0 . lookT (if x == "" then "_" else x)
    blockCode :: Int -> Int -> Int -> Int
    blockCode a b c = 44032 + 588 * a + 28 * b + c


-- Utils

(<$>) :: (a -> b) -> Maybe a -> Maybe b
_ <$> Nothing = Nothing
f <$> Just x  = Just $ f x

(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
(Just f) <*> (Just m) = Just $ f m
_        <*> _        = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing  = a

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r : rs

fromMaybes :: [Maybe a] -> [a]
fromMaybes []     = []
fromMaybes (x:xs) = case x of
  Just a  -> a : fromMaybes xs
  Nothing ->     fromMaybes xs

(<+>) :: Maybe Text -> Maybe Text -> Maybe Text
a <+> b = case (a,b) of
 (Just a', Just b') -> Just $ a' `append` b'
 (_      , _      ) -> Nothing


-- Dictionaries

lookT :: Text -> [(Text,Int,Text)] -> Maybe Int
lookT k l = snd3 <$> find ((== k) . fst3) l

lookI :: Int -> [(Text,Int,Text)] -> Maybe Text
lookI k l = fst3 <$> find ((== k) . snd3) l

-- The third columns in these dictionaries are never used, but they may be in the future.

-- | Maps between a single character and the unicode position of the corresponding jamo.
single :: [(Text, Int, Text)]
single =
  [ ("g"    , 12593, "ㄱ")
  , ("gg"   , 12594, "ㄲ")
  , ("gl"   , 12595, "ㄳ")
  , ("n"    , 12596, "ㄴ")
  , ("lj"   , 12597, "ㄵ")
  , ("lh"   , 12598, "ㄶ")
  , ("d"    , 12599, "ㄷ")
  , ("dd"   , 12600, "ㄸ")
  , ("l"    , 12601, "ㄹ")
  , ("lg"   , 12602, "ㄺ")
  , ("lm"   , 12603, "ㄻ")
  , ("lb"   , 12604, "ㄼ")
  , ("ls"   , 12605, "ㄽ")
  , ("lt"   , 12606, "ㄾ")
  , ("lp"   , 12607, "ㄿ")
  , ("lh"   , 12608, "ㅀ")
  , ("m"    , 12609, "ㅁ")
  , ("b"    , 12610, "ㅂ")
  , ("bb"   , 12611, "ㅃ")
  , ("bs"   , 12612, "ㅄ")
  , ("s"    , 12613, "ㅅ")
  , ("ss"   , 12614, "ㅆ")
  , ("ng"   , 12615, "ㅇ")
  , ("j"    , 12616, "ㅈ")
  , ("jj"   , 12617, "ㅉ")
  , ("ch"   , 12618, "ㅊ")
  , ("k"    , 12619, "ㅋ")
  , ("t"    , 12620, "ㅌ")
  , ("p"    , 12621, "ㅍ")
  , ("h"    , 12622, "ㅎ")
  , ("a"    , 12623, "ㅏ")
  , ("ae"   , 12624, "ㅐ")
  , ("ya"   , 12625, "ㅑ")
  , ("yae"  , 12626, "ㅒ")
  , ("oe"   , 12627, "ㅓ")
  , ("e"    , 12628, "ㅔ")
  , ("yeo"  , 12629, "ㅕ")
  , ("ye"   , 12630, "ㅖ")
  , ("o"    , 12631, "ㅗ")
  , ("wa"   , 12632, "ㅘ")
  , ("wae"  , 12633, "ㅙ")
  , ("wi"   , 12634, "ㅚ")
  , ("yo"   , 12635, "ㅛ")
  , ("u"    , 12636, "ㅜ")
  , ("weo"  , 12637, "ㅝ")
  , ("we"   , 12638, "ㅞ")
  , ("wi"   , 12639, "ㅟ")
  , ("yu"   , 12640, "ㅠ")
  , ("eu"   , 12641, "ㅡ")
  , ("yi"   , 12642, "ㅢ")
  , ("i"    , 12643, "ㅣ")
  -- 12644, "ㅤ"
  , ("nn"   , 12645, "ㅥ")
  , ("nd"   , 12646, "ㅦ")
  , ("ns"   , 12647, "ㅧ")
  -- 12648, "ㅨ"
  , ("lgs"  , 12649, "ㅩ")
  , ("ld"   , 12650, "ㅪ")
  , ("lbs"  , 12651, "ㅫ")
  -- 12652, "ㅬ"
  , ("lh"   , 12653, "ㅭ")
  , ("mb"   , 12654, "ㅮ")
  , ("ms"   , 12655, "ㅯ")
  -- 12656, "ㅰ"
  , ("mng"  , 12657, "ㅱ")
  , ("bg"   , 12658, "ㅲ")
  , ("bd"   , 12659, "ㅳ")
  , ("bsg"  , 12660, "ㅴ")
  , ("bsd"  , 12661, "ㅵ")
  , ("bj"   , 12662, "ㅶ")
  , ("bt"   , 12663, "ㅷ")
  , ("bng"  , 12664, "ㅸ")
  , ("bbng" , 12665, "ㅹ")
  , ("bg"   , 12666, "ㅺ")
  , ("sn"   , 12667, "ㅻ")
  , ("sd"   , 12668, "ㅼ")
  , ("sb"   , 12669, "ㅽ")
  , ("sj"   , 12670, "ㅾ")
  -- 12671, "ㅿ"
  -- 12672, "ㆀ"
  -- 12673, "ㆁ"
  -- 12674, "ㆂ"
  -- 12675, "ㆃ"
  , ("png"  , 12676, "ㆄ")
  , ("hh"   , 12677, "ㆅ")
  -- 12678, "ㆆ"
  , ("yoya" , 12679, "ㆇ")
  , ("yoyae", 12680, "ㆈ")
  , ("yoi"  , 12681, "ㆉ")
  , ("yuyeo", 12682, "ㆊ")
  , ("yuye" , 12683, "ㆋ")
  , ("yui"  , 12684, "ㆌ")
  ]

-- | Offset for a jamo in the first (consonant) block position.
initial :: [(Text,Int,Text)]
initial =
  [ ("g" ,  0, "&#x1100") -- "kiyeok"      {- ᄀ -}
  , ("gg",  1, "&#x1101") -- "ssangkiyeok"
  , ("n" ,  2, "&#x1102") -- "nieun"
  , ("d" ,  3, "&#x1103") -- "dikeut"
  , ("dd",  4, "&#x1104") -- "ssangdikeut"
  , ("l" ,  5, "&#x1105") -- "rieul"
  , ("m" ,  6, "&#x1106") -- "mieum"
  , ("b" ,  7, "&#x1107") -- "bieup"
  , ("bb",  8, "&#x1108") -- "ssangbieup"
  , ("s" ,  9, "&#x1109") -- "siot"
  , ("ss", 10, "&#x110A") -- "ssangsiot"
  , ("_" , 11, "&#x110B") -- "ieung"
  , ("j" , 12, "&#x110C") -- "jieut"
  , ("jj", 13, "&#x110D") -- "ssangjieut"
  , ("ch", 14, "&#x110E") -- "chieuch"
  , ("k" , 15, "&#x110F") -- "kieuk"
  , ("t" , 16, "&#x1110") -- "thieuth"     {- ㅌ -}
  , ("p" , 17, "&#x1111") -- "phieuph"
  , ("h" , 18, "&#x1112") -- "hieuh"
  ]

-- | Offset for a jamo in the second (vowel) block position.
vowel :: [(Text,Int,Text)]
vowel =
  [ ("a"  ,  0, "&#x1161")
  , ("ae" ,  1, "&#x1162")
  , ("ya" ,  2, "&#x1163")
  , ("yae",  3, "&#x1164")
  , ("eo" ,  4, "&#x1165")
  , ("e"  ,  5, "&#x1166")
  , ("yeo",  6, "&#x1167")
  , ("ye" ,  7, "&#x1168")
  , ("o"  ,  8, "&#x1169")
  , ("wa" ,  9, "&#x116A")
  , ("wae", 10, "&#x116B")
  , ("oe" , 11, "&#x116C")
  , ("yo" , 12, "&#x116D")
  , ("u"  , 13, "&#x116E")
  , ("weo", 14, "&#x116F")
  , ("we" , 15, "&#x1170")
  , ("wi" , 16, "&#x1171")
  , ("yu" , 17, "&#x1172")
  , ("eu" , 18, "&#x1173")
  , ("yi" , 19, "&#x1174")
  , ("i"  , 20, "&#x1175")
  ]

-- | Offset for a jamo in the last (consonant) block position.
final :: [(Text,Int,Text)]
final =
  [ ("_"  ,  0, ""       ) -- "nothing"
  , ("g"  ,  1, "&#x11A8") -- "kiyeok"
  , ("gg" ,  2, "&#x11A9") -- "ssangkiyeok"
  , ("gs" ,  3, "&#x11AA") -- "kiyeok-siot"
  , ("n"  ,  4, "&#x11AB") -- "nieun"
  , ("nj" ,  5, "&#x11AC") -- "nieun-chieut"
  , ("nh" ,  6, "&#x11AD") -- "nieun-hieuh"
  , ("d"  ,  7, "&#x11AE") -- "tikeut"
  , ("l"  ,  8, "&#x11AF") -- "rieul"
  , ("lg" ,  9, "&#x11B0") -- "rieul-kiyeok"
  , ("lm" , 10, "&#x11B1") -- "rieul-mieum"
  , ("lb" , 11, "&#x11B2") -- "rieul-bieup"
  , ("ls" , 12, "&#x11B3") -- "rieul-siot"
  , ("lt" , 13, "&#x11B4") -- "rieul-thieuth"
  , ("lp" , 14, "&#x11B5") -- "rieul-phieuph"
  , ("lh" , 15, "&#x11B6") -- "rieul-hieuh"
  , ("m"  , 16, "&#x11B7") -- "mieum"
  , ("b"  , 17, "&#x11B8") -- "bieup"
  , ("bs" , 18, "&#x11B9") -- "bieup-siot"
  , ("s"  , 19, "&#x11BA") -- "siot"
  , ("ss" , 20, "&#x11BB") -- "ssangsiot"
  , ("ng" , 21, "&#x11BC") -- "ieung"
  , ("j"  , 22, "&#x11BD") -- "cieuc"
  , ("ch" , 23, "&#x11BE") -- "chieuch"
  , ("k"  , 24, "&#x11BF") -- "kieuk"
  , ("t"  , 25, "&#x11C0") -- "thieuth"
  , ("p"  , 26, "&#x11C1") -- "phieuph"
  , ("h"  , 27, "&#x11C2") -- "hieuh"
  ]
