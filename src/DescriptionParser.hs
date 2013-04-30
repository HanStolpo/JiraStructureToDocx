{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, ScopedTypeVariables, BangPatterns #-}
 -- GHC_STATIC_OPTION_i=../src:../testsuite
module DescriptionParser where

import qualified Data.Char as C
import Control.Monad
import Text.Parsec
import Text.Parsec.Prim
import Text.Pandoc
import Text.Pandoc.Builder hiding (space)
import Test.HUnit
import Control.Applicative ((<$>))
import Debug.Trace
import Data.List

data InlineFmt = InlEmph | InlStrong | InlUnderLine | InlStike
    deriving (Show)

data ParseState = PS
    {
        psLevel :: Int,
        psDepth :: Int,
        psListPrefix :: String,
        psIgnoreChars :: String,
        psInlineFmtStack :: [InlineFmt]
    }deriving (Show)
defaultParseState = PS {psLevel = 0, psDepth = 0, psListPrefix = "", psIgnoreChars = "", psInlineFmtStack = []}

type MyParser = Parsec [Char] ParseState

parseDescription :: Int -> String -> [Block]
-- parseDescription l ds = Para [Str $ filter (\a -> a /= '\r') ds ]
parseDescription l ds = 
    case res of
        Left e -> [RawBlock "" (show e)]
        Right b -> b
    where
        res = runParser prsDesc  defaultParseState {psLevel = l} "Desc" ds

prsDesc = prsBlocks1
    
trace' !msg !s !a  = trace ("\nt-" ++ msg ++ " -> " ++ (show s) ++ "\n") $! a 
trace'' !msg !a  = trace' msg a a
---------------------------------------------------------------------------------------------
-- Block parsing
---------------------------------------------------------------------------------------------

prsBOL :: MyParser ()
prsBOL = do
    src <-  getPosition
    let cn = sourceColumn src 
    if cn == 1 then return () else fail "expecting beginning of line"

prsBOF :: MyParser ()
prsBOF = do
    src <-  getPosition
    let cn = sourceColumn src 
    let ln = sourceLine src 
    if cn == 1 && ln == 1 then return () else fail "expecting beginning of file"


prsBlocks :: MyParser [Block]
prsBlocks = many $ choice prsBlockOrder
prsBlocks1 = many1 $ choice prsBlockOrder
    
prsBlockOrder = map try [prsEmptyLines, prsTable, prsHdng, prsHorizRule, prsNumberedList, prsBulletList, prsParagraph
                        ]

prsIsBlockStart :: MyParser ()
prsIsBlockStart = do
    lookAhead $ choice $ map try [prsAnyNewListPfxS
                                 ,prsHorizRuleS
                                 ,prsHdngPfxS
                                 ,prsTableHdrStartS
                                 ,prsEmptyLines >> return ""
                                 ,prsTableCellStartS
                                 ,prsTableRowsStartS
                                 ,eof >> return ""]
    return ()
                                               

prsIsNotBlockStart :: MyParser ()
prsIsNotBlockStart = (prsIsBlockStart >> fail "Not expecting block start") <|> (return ())

prsIsBlockStartB :: MyParser Bool
prsIsBlockStartB = (prsIsBlockStart >> return True) <|> (return False)

prsRestOfLine = optional (blankline <|> (skipSpaces >> prsPassIgnoreChar'))


prsEmptyLines = many1 (   (newline >> many spaceChar >> newline)  -- empty lines or file starts with newlines
                      <|> (prsBOF >> many spaceChar >> newline)   -- file starts with one empty line
                      ) >> return Null

prsHdngPfx :: MyParser (String, Int)
prsHdngPfx = do
    prsBOL
    skipSpaces
    h <- oneOf "hH"
    d <- digit 
    p <- oneOf "."
    many1 spaceChar
    return ([h,d,p], C.digitToInt d)

prsHdngPfxS = (liftM fst) prsHdngPfx

prsHdng :: MyParser Block
prsHdng = do
    l <- liftM psLevel $ getState
    d <- (liftM snd) prsHdngPfx
    inls <- prsInlsTillEol >>~ prsRestOfLine
    return $ Header (d + l) nullAttr inls

prsHorizRuleS :: MyParser String
prsHorizRuleS = do
    prsBOL
    s <- string "----"
    s' <- many (char '-')
    prsRestOfLine
    return $ s ++ s'

prsHorizRule :: MyParser Block
prsHorizRule = do
    prsHorizRuleS
    return HorizontalRule

prsParagraph :: MyParser Block
prsParagraph = prsInlsTillBlock1 >>= toPara
    where 
        toPara inls = return $ Para $ dropTailSpace inls
        dropTailSpace inls = reverse . (dropWhile matchSpace) . reverse $ inls
        matchSpace Space = True
        matchSpace _     = False
{-prsParagraph =  do-}
    {-bStop <- prsStopList2-}
    {-if bStop then fail "" else return ()-}
    {-pfx <- liftM psListPrefix $ getState-}
    {-inlss <- _prsParagraph pfx []-}
    {-return $  Para (concat $ intersperse [Space] inlss)-}
    {-where -}
        {-_prsParagraph pfx lss = do-}
            {-!lss' <- many1 prsInlsTillEol1-}
            {-let lss'' = lss ++ lss'-}
            {-b <-  option True prsBlankLines -}
            {-if not b then prsRestOfLine <|> return () else return ()-}
            {-bStop <- option True  _prsStopListPara-}
            {-if  b ||  bStop then return lss'' else _prsParagraph pfx lss''-}
            {-where-}
                {-_prsStopListPara = do-}
                    {-let prsNewList = many1Till (oneOf "#*-") spaceChar -}
                    {-r <- optionMaybe $ lookAhead $ choice $ map try [prsNewList, prsHorizRuleS, prsHdngPfxS, prsTableHdrStartS, liftM (:"") prsPassIgnoreChar' , eof >> return ""]-}
                    {-case r of -}
                        {-Nothing -> return False-}
                        {-Just s -> return True-}


prsAnyNewListPfxS = prsBOL >> skipSpaces >> many1 (oneOf "*#-") >>~ spaceChar

prsLstContent = many (choice $ map try [prsNumberedList, prsBulletList, prsParagraph, prsTable])  

prsBulletList :: MyParser Block
prsBulletList = do
    let prsPrefix = (\pfx -> oneOfStrings [ pfx ++ "*", pfx ++ "-" ] >>~ spaceChar >>~ skipSpaces)
    let maker = BulletList  
    prsAnyListList maker prsPrefix

getListFmt :: MyParser ListNumberStyle
getListFmt = do
    ps <- getState
    let pfxDepth = psDepth ps `mod` 4
    case pfxDepth of
        0 -> return LowerAlpha
        1 -> return LowerRoman
        2 -> return UpperAlpha
        3 -> return UpperRoman

prsNumberedList :: MyParser Block
prsNumberedList = do
    lstFmt <- getListFmt
    let prsPrefix = (\pfx -> string ( pfx ++ "#") >>~ spaceChar >>~ skipSpaces)
    let maker = OrderedList (1, lstFmt, DefaultDelim)  
    prsAnyListList maker prsPrefix

prsAnyListList :: ([[Block]] -> Block) -> (String -> MyParser String) ->  MyParser Block
prsAnyListList maker prsPrefix = do
    -- Match beginning of line
    prsBOL
    -- bail out of prsLstContent
    bStop <- prsStopList2
    if bStop then fail "" else return ()
    -- Read list starter and set list prefix if successful
    ps <- getState
    let pfx =  psListPrefix ps
    let pfxDepth = psDepth ps
    let prsPrefix' = prsPrefix pfx
    s <- prsPrefix'
    setState $ ps {psListPrefix = s, psDepth = pfxDepth  + 1}
    -- Skip any leading spaces
    skipSpaces
    -- Recursively consume lists for this level
    bss <- _prsList prsPrefix' [] 
    -- Reset list prefix
    setState $ ps
    -- Create block
    return $ maker bss 
    where
        _prsList prsPrefix bss' = do
            -- get the list content 
            bss'' <- prsLstContent
            c <- optionMaybe $ lookAhead $ try $ prsPrefix
            case c of
                 -- It does not match so return current content
                 Nothing -> return $  bss' ++ [bss'']
                 -- It does match consume start and then call self to get contetn
                 Just _ -> prsPrefix >> (_prsList prsPrefix $ bss' ++ [bss''])

prsListPfxDepth :: MyParser Bool
prsListPfxDepth = do
    let prsPrefix = (many1 $ oneOfStrings [ "#", "*", "-" ]) >>~ spaceChar
    r <- optionMaybe $ lookAhead $ try $ prsPrefix
    case r of
        Nothing -> return False
        Just pfx -> do
                    let d = length pfx
                    ps <- getState
                    let pfxDepth =  psDepth ps
                    return $ d <= pfxDepth

prsBlankLines :: MyParser Bool
prsBlankLines = do
    r <- optionMaybe $ lookAhead $ try $ blanklines
    case r of
        Nothing -> return False
        Just s -> return $ length s > 1

prsStopList2 :: MyParser Bool
prsStopList2 = do
     b <- prsListPfxDepth
     b' <- prsBlankLines
     if b || b' then return True
         else do
             r <- optionMaybe $ lookAhead $ choice $ map try [prsHorizRuleS, prsHdngPfxS]
             case r of 
                 Nothing -> return False
                 Just s -> return True

prsTableContent = many (choice $ map try [prsParagraph])  

prsTable :: MyParser Block
prsTable = do
    hdr <- prsTableHdr
    prsRestOfLine
    rows <-  many prsTableRows
    let aln = map (\_->AlignLeft) hdr
    let colw = map (\_->0) hdr
    let cap = [Str ""]
    return $  Table cap aln colw hdr rows


prsTableHdrStartS :: MyParser String
prsTableHdrStartS = do
    prsBOL
    skipSpaces >> string "||"

prsTableHdr :: MyParser [TableCell]
prsTableHdr = do
    prsTableHdrStartS
    updateState (\ps -> ps {psIgnoreChars =  '|':(psIgnoreChars ps)}) 
    r <- endBy1 prsTableContent $ string "||"
    updateState (\ps -> let (i:is) = psIgnoreChars ps in ps {psIgnoreChars = is }) 
    return r


prsTableRowsStartS :: MyParser String
prsTableRowsStartS = do
    i <- liftM (psIgnoreChars) getState
    case i of
        '|':_ -> ((newline >> return ()) <|> prsBOL) >> skipSpaces >> string "|"
        _ -> fail "Not inside table" >> return ""

prsTableCellStartS :: MyParser String
prsTableCellStartS = do
    i <- liftM (psIgnoreChars) getState
    case i of
        '|':_ -> string "|"
        _ -> fail "Not inside table" >> return ""

prsTableRows :: MyParser [TableCell]
prsTableRows = do
    prsBOL
    s <- skipSpaces >> string "|"
    updateState (\ps -> ps {psIgnoreChars =  '|':(psIgnoreChars ps)}) 
    r <- endBy1 prsTableContent $ string "|"
    prsRestOfLine
    updateState (\ps -> let (i:is) = psIgnoreChars ps in ps {psIgnoreChars = is }) 
    return r
    
    
---------------------------------------------------------------------------------------------
-- Inline parsing
---------------------------------------------------------------------------------------------
addLineBreakBeforeAfterImages :: [Inline] -> MyParser [Inline]
addLineBreakBeforeAfterImages ls = return $ foldr f [] $ ls
    where
        f img@(Image _ _) ls' = LineBreak : img : LineBreak : ls'
        f l ls' = l : ls'

prsInlsTillEol :: MyParser [Inline]
prsInlsTillEol =  (many  (prsFailIgnoreChar >> choice prsInlineOrder) ) >>= addLineBreakBeforeAfterImages
prsInlsTillEol1 = (many1  (prsFailIgnoreChar >> choice prsInlineOrder) ) >>= addLineBreakBeforeAfterImages

prsInlineOrder = map try [  prsLongDash, prsMediumDash, prsEmph, prsStrong, prsCitation, prsDeleted, prsInserted, prsSuperScript,
                            prsSubScript, prsMonoSpaced, prsImage, prsLineBreak,  prsWord, prsSpace
                         ]

prsInlineOrder2 :: [MyParser Inline]
prsInlineOrder2 = map try [ prsLongDash, prsMediumDash, prsEmph, prsStrong, prsCitation, prsDeleted, prsInserted, prsSuperScript,
                            prsSubScript, prsMonoSpaced, prsImage, prsLineBreak,  prsWord, prsSpaceTillBlock, 
                            (newline >> return Space)
                          ]

prsInlsTillBlock :: MyParser [Inline]
prsInlsTillBlock = (liftM reverse $ prsInlsTillBlock1' []) >>= addLineBreakBeforeAfterImages
    where 
        prsInlsTillBlock1' ils = do
            stop <-  prsIsBlockStartB
            if stop 
                then do 
                    return ils
                else do 
                    prsFailIgnoreChar
                    !il <- choice prsInlineOrder2
                    prsInlsTillBlock1' (il:ils)

prsInlsTillBlock1 = prsInlsTillBlock >>= (\ils -> if null ils then fail "Expected at least one Inline" else return ils)
    
    

prsEnclosed s e = enclosed (s >> notFollowedBy space) (lookAhead (try nonspaceChar) >> e)
prsEnclosed' s  = prsEnclosed s s

prsAnyChar :: MyParser Char
prsAnyChar = do
    ignr <- liftM psIgnoreChars getState
    noneOf ignr
    
prsFailIgnoreChar' :: MyParser Char
prsFailIgnoreChar' = do
    ignr <- liftM psIgnoreChars getState
    lookAhead $ try $  satisfy (\c -> not $ elem c ignr)

prsFailIgnoreChar :: MyParser ()
prsFailIgnoreChar = do
    prsFailIgnoreChar'
    return ()

prsPassIgnoreChar' :: MyParser Char
prsPassIgnoreChar' = do
    ignr <- liftM psIgnoreChars getState
    lookAhead $ try $  satisfy (\c -> elem c ignr)

prsPassIgnoreChar :: MyParser ()
prsPassIgnoreChar = do
    prsPassIgnoreChar'
    return ()

prsNotEol = prsAnyChar >>~ notFollowedBy newline

-- _emphasis_
prsEmph :: MyParser Inline
prsEmph = do
    s <- prsEnclosed' (string "_") prsNotEol
    return $ Emph [Str s]

-- *strong*
prsStrong :: MyParser Inline
prsStrong = do
    s <- prsEnclosed' (string "*") prsNotEol
    return $ Strong [Str s]

-- ??citation??
prsCitation :: MyParser Inline
prsCitation = do
    s <- prsEnclosed' (string "??") prsNotEol
    return $ Cite [Citation s [] [] NormalCitation 0 0] [Str s]

-- -deleted-
prsDeleted :: MyParser Inline
prsDeleted = do
    s <- prsEnclosed' (string "-") prsNotEol
    return $ Strikeout [Str s]

-- +inserted+
prsInserted :: MyParser Inline
prsInserted = do
    s <- prsEnclosed' (string "+") prsNotEol
    return $ SmallCaps [Str s]

-- ^superscript^
prsSuperScript :: MyParser Inline
prsSuperScript = do
    s <- prsEnclosed' (string "^") prsNotEol
    return $ Superscript [Str s]

-- ~subscript~
prsSubScript :: MyParser Inline
prsSubScript = do
    s <- prsEnclosed' (string "~") prsNotEol
    return $ Subscript [Str s]

-- {{monospaced}}
prsMonoSpaced :: MyParser Inline
prsMonoSpaced = do
    s <- prsEnclosed' (string "{{") prsNotEol
    return $ Code nullAttr s

-- \\ line break
prsLineBreak :: MyParser Inline
prsLineBreak = string "\\\\" >> notFollowedBy' (string "\\") >> (return $ LineBreak)


-- -- medium dash en dash U+2013
prsMediumDash :: MyParser Inline
prsMediumDash = do
    string "--"
    return $ Str "\2013"

-- --- long dash em dash U+2014
prsLongDash :: MyParser Inline
prsLongDash = do
    string "---"
    return $ Str "\2014"    


-- any normal word
prsWord :: MyParser Inline
prsWord = do
    ignr <- liftM psIgnoreChars getState
    s <- many1 $ noneOf $ ignr ++ " \t\n\r"
    return $ Str  s

-- spaces
prsSpace :: MyParser Inline
prsSpace = do
    s <- many1 spaceChar
    notFollowedBy' (eof <|> (newline >> return ()))
    return  Space

prsSpaceTillBlock :: MyParser Inline
prsSpaceTillBlock = many1 ( prsIsNotBlockStart >> (spaceChar <|> newline) ) >> return Space

prsText :: MyParser [Inline]
prsText = do
    tss <- manyTill (try (do s <- prsSpace; t <- prsWord; return [s,t] ) <|> (do t <- prsWord; return [t] )) prsEndl
    return $ concat $ ( tss ++ [[Space]] )

prsEndl :: MyParser ()
prsEndl = do
    manyTill space $ lookAhead $ try newline
    newline
    return ()

prsImage :: MyParser Inline
prsImage = do
    ImageLink imgL _ <- prsImageRaw
    return $ Image [Str imgL] (imgL, "")

-- parse an image link
data ImageLink = ImageLink 
    {
        imgLink :: String,
        imgAttrs :: Maybe String
    }deriving (Show, Eq)

prsImageRaw :: MyParser ImageLink
prsImageRaw = do
    s <- prsEnclosed' (string "!") prsNotEol
    (l,a) <- parseFromString _prsLinkAttr s
    return $ ImageLink l a
    where
        _prsLinkAttr = do
            l <- many $ noneOf "|"
            a <- optionMaybe $ (char '|' >> (many anyChar))
            return (l,a)

prsImagesRaw :: MyParser [ImageLink]
prsImagesRaw = do
    liftM concat $ many $ (liftM (:[]) $ try prsImageRaw) <|> (anyChar >> return [])

imagesFromDescription :: String -> [ImageLink]
imagesFromDescription d = 
    case r of
        Left e -> error $ show e
        Right a -> a
    where
        r = runParser prsImagesRaw defaultParseState "" d 

replaceImageLinks :: (ImageLink -> ImageLink) -> String -> String
replaceImageLinks _ [] = []
replaceImageLinks replacer d =
    case r of
        Left e -> error $ show e
        Right a -> foldl collapse "" a
    where
        r = runParser p defaultParseState "" d 
        -- Parse image links and prefixes
        p :: MyParser [Either ImageLink String]
        p = many pe
        pe :: MyParser (Either ImageLink String)
        pe = pImage <|> pText 
            where
                pImage = liftM Left $ try $ prsImageRaw 
                pText =  liftM Right $ many1 pTextChar
                pTextChar = (lookAhead $ try $ pNotImage) >> anyChar
                pNotImage = (prsImageRaw >> fail "" ) <|> (return ())
        -- Transform parse results and turn into string again
        collapse :: String -> Either ImageLink String -> String 
        collapse sAcc (Left iml ) = let ImageLink m as = replacer iml in sAcc ++ "!" ++ m ++ (maybe "" (\a-> "|" ++ a) as) ++ "!"
        collapse sAcc (Right s) = sAcc ++ s

----------------------------------------------------------------------------------------
-- HUnit parser tests
----------------------------------------------------------------------------------------
testPS s p i = 
    case r of 
        Left e -> Left $ show e ++ " for input : " ++ i
        Right a -> Right a
    where
        r = runParser p s "" i

testP = testPS defaultParseState

testPrsLeftOver :: MyParser String
testPrsLeftOver = many anyChar


matchError (Right a) = trace (show a) False
matchError (Left a) = True

descriptionParserTests = test
        [   "newline" ~: Right '\n'  ~=? testP newline "\n",

            "prsBOF" ~:  True ~=? (matchError $ testP (anyChar >> prsBOF) "a\n"),
            "prsBOF 2" ~:  Right () ~=? testP (prsBOF) "a\n",

            "prsEmptyLines" ~: Right Null ~=? testP prsEmptyLines "\n\n",
            "prsEmptyLines 2" ~: Right Null ~=? testP prsEmptyLines "\n \n",
            "prsEmptyLines 3" ~:  True ~=? (matchError $ testP (anyChar >> prsEmptyLines) "a\n"),

            "prsIsBlockStartB" ~: Right True ~=? testP prsIsBlockStartB "\n\n",
            "prsIsBlockStartB 2" ~: Right True ~=? testP prsIsBlockStartB "\n \n",
            "prsIsBlockStartB 3" ~: Right False ~=? testP (anyChar >> prsIsBlockStartB) "a\n",
            
            "prsInlsTillBlock1" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text _emp_",
            "prsInlsTillBlock1 2" ~: Right (toList $ text "some text " <> emph "emp") ~=? testP prsInlsTillBlock1 "some text\n_emp_",
            "prsInlsTillBlock1 3" ~: Right (toList $ text "some text") ~=? testP prsInlsTillBlock1 "some text\n\n_emp_",
            "prsInlsTillBlock1 4" ~: Right ([Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"]) 
                          ~=? testP prsInlsTillBlock1 "Henry the\nbig bad \nwolf",
--{--                
            "prsEndl" ~: Right ()  ~=? testP prsEndl "   \n \n",
            "prsSpace" ~: Right Space ~=? testP prsSpace " i",
            "(prsSpace >> testPrsLeftOver)" ~: Right "i" ~=? testP (prsSpace >> testPrsLeftOver) " i",
            "prsWord 1" ~: Right (Str "blah") ~=? testP prsWord "blah blah  ",
            "prsWord 2" ~: Right " blah  " ~=? testP (prsWord >> testPrsLeftOver) "blah blah  ",

            "prsMediumDash" ~: Right (Str "\2013") ~=? testP prsMediumDash "--",
            "prsLongDash" ~: Right (Str "\2014") ~=? testP prsLongDash "---",

            "prsHorizRule 1" ~: Right HorizontalRule ~=? testP prsHorizRule "----",
            "prsHorizRule 2" ~: Right HorizontalRule ~=? testP prsHorizRule "-----",
            "prsHorizRule 3" ~: Right HorizontalRule ~=? testP prsHorizRule "------",
            "prsHorizRule 4" ~: Right HorizontalRule ~=? testP prsHorizRule "-------\n",

            "prsLineBreak 1" ~: Right LineBreak ~=? testP prsLineBreak "\\\\",

            "prsEmph 1" ~: Right (Emph [Str "blah"]) ~=? testP prsEmph "_blah_",

            "prsStrong 1" ~: Right (Strong [Str "blah"]) ~=? testP prsStrong "*blah*",

            "prsStrong 1" ~: Right (Superscript [Str "blah"]) ~=? testP prsSuperScript "^blah^",

            "prsStrong 1" ~: Right (Subscript [Str "blah"]) ~=? testP prsSubScript "~blah~",

            "prsInlsTillEol 1" ~: Right [ Subscript[Str "blah"], Str "bleh"] ~=? testP prsInlsTillEol "~blah~bleh",
            "prsInlsTillEol 2" ~: Right [ Subscript[Str "blah"], Space, Str "bleh"] ~=? testP prsInlsTillEol "~blah~   bleh ",
            "prsInlsTillEol 3" ~: Right [ Subscript[Str "blah"], Space, Str "bleh"] ~=? testP prsInlsTillEol "~blah~   bleh \nJhonny",
            "prsInlsTillEol 4" ~: Right " \nJhonny" ~=? testP (prsInlsTillEol >> testPrsLeftOver) "~blah~   bleh \nJhonny",

            "prsHdng" ~: Right (1, [Str "Henry"]) ~=? (\(Header i _ c) -> (i, c)) <$> testP prsHdng "h1. Henry",

            "prsParagraph" ~: Right (Para [Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"]) 
                          ~=? testP prsParagraph "Henry the\nbig bad \nwolf",

            "prsParagraph" ~: Right (Para [Str "Henry", Space, Str "the", Space, Str "big", Space, Str "bad", Space, Str "wolf"]) 
                           ~=? testP prsParagraph "Henry the\nbig bad \nwolf\n\n",

            "prsBulletList" ~: Right (BulletList [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ]  ]) 
                            ~=? testP prsBulletList "* Gordon *gordon*\n* Ramsy",
        
            "prsNumberedList" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ] ]) 
                            ~=? testP prsNumberedList "# Gordon *gordon*\n# Ramsy",
                
            "prsNumberedList 2" ~: Right  [(OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ], [ Para [Str "Ramsy"] ] ])]
                             ~=? testP prsBlocks "# Gordon *gordon*\n# Ramsy",

            "prsNumberedList 3" ~: Right [ OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]] ]  ]
                                         , Header 1 nullAttr [ Str "Ramsy" ]  
                                         ] 
                             ~=? testP prsBlocks "# Gordon *gordon*\nh1. Ramsy",

            "prsNumberedList 4" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon", Space, Strong [Str "gordon"]], OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Ramsy"] ] ] ]  ]) 
                            ~=? testP prsNumberedList "# Gordon *gordon*\n## Ramsy",

            "prsNumberedList 5" ~: Right (OrderedList (1, LowerAlpha, DefaultDelim) [  [ Para [Str "Gordon"] , OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Ramsy"] ] ] ]
                                                                                      ,  [ Para [Str "Sally"] ,  OrderedList (1, LowerRoman, DefaultDelim)[ [ Para [Str "Blah"] ] ] ] 
                                                                                     ]) 
                            ~=? testP prsNumberedList "# Gordon\n## Ramsy\n# Sally\n## Blah",

            "prsStopList2 1" ~: Right False ~=? testP prsStopList2 "# blah my end", 
            "prsStopList2 2" ~: Right True ~=? testP prsStopList2 "h1. blah my end", 
            "prsStopList2 4" ~: Right True ~=? testP prsStopList2 "--------------------", 
            "prsStopList2 5" ~: Right True ~=? testPS defaultParseState {psDepth = 1, psListPrefix = "#"} prsStopList2 "# blah my end", 

  --}      
            "prsTable" ~: Right ( Table [Str ""] [AlignLeft,AlignLeft] [0.0,0.0] [ [ Para [Str "Gordon"]], [ Para [Str "Ramsy"]] ] 
                                               [ [[ Para [Str "Sally"]], [Para [Str "Storm"]] ]
                                               , [[ Para [Str "Blah"]], [Para [Str "Bleh"]] ]
                                               ]) 
                            ~=? testP prsTable "||Gordon||Ramsy||\n|Sally|Storm|\n|Blah|Bleh|",

            "prsImageRaw 1" ~: Right (ImageLink "blah.png" Nothing) ~=? testP prsImageRaw "!blah.png!", 

            "prsImageRaw 2" ~: Right (ImageLink "blah.png" (Just "a=b, c=d")) ~=? testP prsImageRaw "!blah.png|a=b, c=d!", 

            "prsImageRaw 3" ~: Right (ImageLink "http://srv1.za.5dt.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png" (Just "caption")) ~=? testP prsImageRaw "!http://srv1.za.5dt.com:8090/secure/attachment/12175/ATSP3_CFI_Gathered%20Media_Lynx_Helicopter_Instrumentation_InterseatCon_CDF.png|caption!", 

            "imagesFromDescription 1" ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}] ~=? imagesFromDescription "gordon !bla! ramsy",
            "imagesFromDescription 2" ~: [ImageLink {imgLink = "bla", imgAttrs = Nothing}, ImageLink {imgLink = "ble", imgAttrs = Nothing}] ~=? imagesFromDescription "gordon !bla! ramsy!ble!",

            "replaceImageLinks 1" ~: "gordon !Sally! ramsy" ~=? replaceImageLinks (\x->ImageLink {imgLink = "Sally", imgAttrs = Nothing}) "gordon !bla! ramsy",

            "dummy end" ~: True ~=? True
        ]


------------------------------------------------------------------------
-- From Pandoc.Parsing
------------------------------------------------------------------------

-- | Like @manyTill@, but reads at least one item.
many1Till :: Parsec [tok] st a
          -> Parsec [tok] st end
          -> Parsec [tok] st [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)
-- | Parses a space or tab.
spaceChar :: Parsec [Char] st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parsec [Char] st Char
nonspaceChar = satisfy $ \x -> x /= '\t' && x /= '\n' && x /= ' ' && x /= '\r'

-- | Skips zero or more spaces or tabs.
skipSpaces :: Parsec [Char] st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Parsec [Char] st Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Parsec [Char] st [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: Parsec [Char] st t   -- ^ start parser
         -> Parsec [Char] st end  -- ^ end parser
         -> Parsec [Char] st a    -- ^ content parser (to be used repeatedly)
         -> Parsec [Char] st [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: [Char] -> Parsec [Char] st String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (C.toUpper x) <|> char (C.toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Parsec [tok] st a -> [tok] -> Parsec [tok] st a
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x


-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => Parsec [a] st b -> Parsec [a] st ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())


oneOfStrings' :: (Char -> Char -> Bool) -> [String] -> Parsec [Char] st String
oneOfStrings' _ []   = fail "no strings"
oneOfStrings' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> fail "not found"
       _   -> (c:) `fmap` oneOfStrings' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: [String] -> Parsec [Char] st String
oneOfStrings = oneOfStrings' (==)
