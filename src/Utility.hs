{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Utility   ( prettyJson
                    ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack)     -- only import string instances for overloaded strings
import qualified Data.ByteString.Lazy.Char8 as C 
import qualified Data.Char as C
import Data.ByteString.Lazy.Builder
import Data.Monoid
--import Data.Default


prettyJson :: C.ByteString -> C.ByteString
prettyJson = toLazyByteString . toB 
    where
        -- select how builder is created
        toB :: ByteString -> Builder
        toB s | C.length s < 2 = lazyByteString  s
        toB s' = let s = unpack s' in (char8 . C.head $ s') <> (fst . foldl f (mempty, ("  ", False)) $ zip s (drop 1 s))
        -- transform chars
        f :: (Builder, (String, Bool)) -> (Char, Char) -> (Builder, (String, Bool))
        f b@(_, (_, False)) (_, c) | C.isSpace c = b
        f (b, (pfx, False)) (p, c) = let (pfx', pfx'') = m c pfx in (b <> nl p c pfx', (pfx'', fis p c False)) 
        f (b, (pfx, True)) (p, c)  = (b <> char8 c, (pfx, fis p c True)) 
        -- mod the pfx
        m c pfx | c `elem` "{[" = (pfx, ' ':' ':pfx)
        m c (_:_:pfx') | c `elem` "}]" = (pfx', pfx')
        m _ pfx = (pfx, pfx) 
        -- new line choice
        nl :: Char -> Char -> String -> Builder
        nl p c pfx | p /= ',' && c `elem` "{[]}" = char8 '\n' <> string8 pfx <> char8 c
        nl p c pfx | p == ',' && notElem c "{[]}" = char8 '\n' <> string8 pfx <> char8 c
        nl _ c _ = char8 c
        -- flip in string status
        fis '\\' '"' b = b
        fis _ '"' b = not b
        fis _ _ b = b



