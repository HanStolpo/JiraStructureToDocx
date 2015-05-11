{-# LANGUAGE OverloadedStrings, TupleSections, QuasiQuotes #-}
  
module Jira.Utility ( prettyJson
                    , unflattenPandoc
                    , unflattenHierarchy
                    , correctHdrLevels
                    , correctHdrLevels'
                    , jiraUtilityTestGroup
                    ) where

import Data.ByteString.Lazy.Char8 (ByteString, unpack)     -- only import string instances for overloaded strings
import qualified Data.ByteString.Lazy.Char8 as C 
import qualified Data.Char as C
import Data.ByteString.Lazy.Builder
import Data.Monoid
import Data.Tree
import Data.List
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Pandoc.Definition
import Text.RawString.QQ
--import Data.Default
-- local
import Pandoc.Reader.Jira



-- takse an input string assumed to be representing JSON and format it for easier viewing adding newlines and spaces
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



-- Take a list of values and clasification function and an ordering function and turn it into a Hierarchical Tree
unflattenHierarchy :: (a -> Bool)           -- is the value a parent value
                   -> (a -> a -> Bool)      -- order parent values (parents can only be added to lower level parents)
                   -> a                     -- the root value in the tree
                   -> [a]                   -- the list to convert to hierarchical tree
                   -> Tree a
unflattenHierarchy isPrnt ordPrnt rv as = extBot . collapse rv . foldl' step [Node rv []] $ as
    where
        extBot [Node n sf] = Node n (reverse sf)
        extBot _ = error "unflattenHierarchy - error should be at bottom"

        addChildToTop _ [] = error "unflattenHierarchy - no element at top of stack"
        addChildToTop c (p:st) = p{subForest = c : subForest p} : st

        collapse _ []  = []
        collapse _ [a] = [a]
        collapse a st@(Node b bsf : st') 
                        | not(b `ordPrnt` a)  = collapse a (addChildToTop (Node b (reverse bsf)) st')
                        | otherwise  = st

        step st a | isPrnt a = Node a [] : st'
                  | otherwise  = addChildToTop (Node a []) st
                  where st' = collapse a st

-- test case for the unflattenHierarchy helper function
caseUnflattenInts :: Assertion
caseUnflattenInts = assertBool "unflattening did not match" chk
    where 
        nums = [-1, -2, 3, 4, -1, 5, 6, -1, 7] :: [Int]
        buildIntTree = unflattenHierarchy (< 0) (>) 0 
        chk = case buildIntTree nums of
                Node 
                    { rootLabel = 0
                    , subForest =
                        [ Node
                            { rootLabel = -1
                            , subForest = [ Node { rootLabel = -2 , subForest = [Node 3 [], Node 4 []] } ]
                            }                                      
                        , Node { rootLabel = -1, subForest = [Node 5 [], Node 6 []]}
                        , Node { rootLabel = -1, subForest = [Node 7 []] }
                        ]
                    } -> True
                _ -> False

-- Takes a flat list of Pandoc Block elements and turn them into a hierarchical tree according to header levels
-- note root of tree will be a Null block element
unflattenPandoc :: [Block] -> Tree Block
unflattenPandoc = unflattenHierarchy chkPrnt ordPrnt Null
    where
        chkPrnt Header{} = True
        chkPrnt _ = False
        ordPrnt (Header a _ _) (Header b _ _) = a < b
        ordPrnt Null _ = True
        ordPrnt _ _ = False

-- test case and example for unflattenPandoc
caseUnflattenPandoc :: Assertion
caseUnflattenPandoc = assertBool "unflattening did not match" chk
    where
        chk = case unflattenPandoc . parseDescription 0 $ caseUnflattenPandocInputTxt of
            Node
                { rootLabel=Null 
                , subForest=
                    [ Node
                        { rootLabel=Header 4 ("",[],[]) [Str"H4"]
                        , subForest=
                            [ Node
                                { rootLabel = Header 5 ("",[ ],[ ]) [Str"H5" ]
                                , subForest = [Node {rootLabel=Para [Str"PinH5" ],subForest=[]}]
                                }
                            ]
                        }
                    , Node
                        { rootLabel=Header 4 ("",[],[]) [Str"2ndH4"]
                        , subForest = [Node {rootLabel=Para [Str"Pin2ndH4"], subForest= []}]
                        }
                    , Node
                        { rootLabel=Header 4 ("",[ ],[ ]) [Str"3dH4" ]
                        , subForest=
                            [Node
                                { rootLabel=BulletList [ [Para [Str"LAIn3dH4" ] ],[Para [Str"LBIn3dH4" ] ] ]
                                , subForest= []
                                }
                            ]
                        }
                    ]
                } -> True
            _ -> False
-- text input for above test case to be parsed then unflattened
caseUnflattenPandocInputTxt :: String
caseUnflattenPandocInputTxt = [r|
h4. H4
h5. H5
PinH5
h4. 2ndH4
Pin2ndH4
h4. 3dH4
* LAIn3dH4
* LBIn3dH4
|]
        

-- Take a tree representing the pandoc document with the tree rooted with 
-- value Null and parents of value Header and ensure that the header levels
-- start at a certain value and are consecutive
correctHdrLevels' :: Int            -- The lowest header level
                  -> Tree Block     -- The input tree reprsenting the pandoc hierarchy
                  -> Tree Block
correctHdrLevels' topLvl t = go topLvl t
    where
        go l n@(Node {rootLabel = Null, subForest = sf}) = n{subForest = map (go l) sf}
        go l Node {rootLabel = Header _ a b, subForest = sf} = Node {rootLabel = Header l a b, subForest = map (go (l+1)) sf}
        go _ n = n

-- Correct the header levels in a pandoc document ensuring 
-- that header levels start at a certain value and are consecutive
correctHdrLevels :: Int         -- The lowest header level
                 -> [Block]     -- The pandoc document
                 -> [Block]
correctHdrLevels topLvl = flatten . correctHdrLevels' topLvl . unflattenPandoc


-- Export all the test cases to be run externally
jiraUtilityTestGroup ::  Test.Framework.Test
jiraUtilityTestGroup = testGroup "jiraUtilityTestGroup" [ testCase "caseUnflattenInts" caseUnflattenInts
                                                        , testCase "caseUnflattenPandoc" caseUnflattenPandoc
                                                        ]
