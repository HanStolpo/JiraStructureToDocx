-- {-# LANGUAGE OverloadedStrings
--   #-}
module Utility   ( prettyJson
                    ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack, pack)     -- only import string instances for overloaded strings
import qualified Data.Char as C

prettyJson :: ByteString -> ByteString
prettyJson s' = pack . reverse . snd . foldl f (("", ""), "") . unpack $ s'
    where
        f ((pfx,     nl), s) '{'= ((pfx ++ "  ", "\n"), "{" ++ pfx ++ nl ++ s)
        f ((_:_:pfx, nl), s) '}'= ((pfx        , "\n"), "}" ++ pfx ++ nl ++ s)
        f ((pfx,     nl), s) '['= ((pfx ++ "  ", "\n"), "[" ++ pfx ++ nl ++ s)
        f ((_:_:pfx, nl), s) ']'= ((pfx        , "\n"), "]" ++ pfx ++ nl ++ s)
        f ((pfx,      _), s) ','= ((pfx        , ""), "\n," ++ s)
        f ((pfx,   ""), s) c 
                    | C.isSpace c = ((pfx, ""), c:s)
                    | otherwise   = ((pfx, "\n"), (c:pfx) ++ s)
        f ((pfx,   nl), s) c = ((pfx, nl), c:s)
