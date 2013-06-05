-- DocX writer copied from pandoc so that it may be hacked at for some custom mods
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module DocxCustom ( writeDocxCustom ) where -- module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Data.List ( intercalate, groupBy )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8
import Codec.Archive.Zip
import Data.Time.Clock.POSIX
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import ImageSize
import Text.Pandoc.Shared hiding (Element)
import Text.Pandoc.Options
import Text.Pandoc.Readers.TeXMath
import Highlighting ( highlight )
import Text.Highlighting.Kate.Types ()
import Text.XML.Light
import Text.TeXMath
import Control.Monad.State
import Text.Highlighting.Kate
import Data.Unique (hashUnique, newUnique)
import System.Random (randomRIO)
import Text.Printf (printf)
import qualified Control.Exception as E
import System.FilePath (takeExtension)
import MIME (getMimeType)

data WriterState = WriterState{
         stTextProperties :: [Element]
       , stParaProperties :: [Element]
       , stFootnotes      :: [Element]
       , stSectionIds     :: [String]
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, String, Maybe String, Element, B.ByteString)
       , stListLevel      :: Int
       , stListNumId      :: Int
       , stNumStyles      :: M.Map ListMarker Int
       , stLists          :: [ListMarker]
       }

data ListMarker = NoMarker
                | BulletMarker
                | NumberMarker ListNumberStyle ListNumberDelim Int
                deriving (Show, Read, Eq, Ord)

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      , stSectionIds     = []
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stListLevel      = -1
      , stListNumId      = 1
      , stNumStyles      = M.fromList [(NoMarker, 0)]
      , stLists          = [NoMarker]
      }

type WS a = StateT WriterState IO a

showTopElement' :: Element -> String
showTopElement' x = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ showElement x

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) . node (unqual s)

toLazy :: B.ByteString -> BL.ByteString
toLazy = BL.fromChunks . (:[])

-- | Produce an Docx file from a Pandoc document.
writeDocxCustom :: WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO BL.ByteString
writeDocxCustom opts doc@(Pandoc (Meta tit auths date) _) = do
  let datadir = writerUserDataDir opts
  refArchive <- liftM (toArchive . toLazy) $
       case writerReferenceDocx opts of
             Just f  -> B.readFile f
             Nothing -> readDataFile datadir "reference.docx"

  ((contents, footnotes), st) <- runStateT (writeOpenXML opts{writerWrapText = False} doc)
                       defaultWriterState
  epochtime <- floor `fmap` getPOSIXTime
  let imgs = M.elems $ stImages st

  -- we create [Content_Types].xml and word/_rels/document.xml.rels
  -- from scratch rather than reading from reference.docx,
  -- because Word sometimes changes these files when a reference.docx is modified,
  -- e.g. deleting the reference to footnotes.xml or removing default entries
  -- for image content types.

  -- [Content_Types].xml
  let mkOverrideNode (part', contentType') = mknode "Override"
               [("PartName",part'),("ContentType",contentType')] ()
  let mkImageOverride (_, imgpath, mbMimeType, _, _) =
             mkOverrideNode ("/word/" ++ imgpath, maybe "application/octet-stream" id mbMimeType)
  let overrides = map mkOverrideNode
                  [("/word/webSettings.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml")
                  ,("/word/numbering.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml")
                  ,("/word/settings.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml")
                  ,("/word/theme/theme1.xml",
                    "application/vnd.openxmlformats-officedocument.theme+xml")
                  ,("/word/fontTable.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml")
                  ,("/docProps/app.xml",
                    "application/vnd.openxmlformats-officedocument.extended-properties+xml")
                  ,("/docProps/core.xml",
                    "application/vnd.openxmlformats-package.core-properties+xml")
                  ,("/word/styles.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml")
                  ,("/word/document.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml")
                  ,("/word/footnotes.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml")
                  ] ++ map mkImageOverride imgs
  let defaultnodes = [mknode "Default"
              [("Extension","xml"),("ContentType","application/xml")] (),
             mknode "Default"
              [("Extension","rels"),("ContentType","application/vnd.openxmlformats-package.relationships+xml")] ()]
  let contentTypesDoc = mknode "Types" [("xmlns","http://schemas.openxmlformats.org/package/2006/content-types")] $ defaultnodes ++ overrides
  let contentTypesEntry = toEntry "[Content_Types].xml" epochtime
        $ UTF8.fromStringLazy $ showTopElement' contentTypesDoc

  -- word/_rels/document.xml.rels
  let toBaseRel (url', id', target') = mknode "Relationship"
                                          [("Type",url')
                                          ,("Id",id')
                                          ,("Target",target')] ()
  let baserels = map toBaseRel
                    [("http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering",
                      "rId1",
                      "numbering.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles",
                      "rId2",
                      "styles.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings",
                      "rId3",
                      "settings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings",
                      "rId4",
                      "webSettings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable",
                      "rId5",
                      "fontTable.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme",
                      "rId6",
                      "theme/theme1.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes",
                      "rId7",
                      "footnotes.xml")]
  let toImgRel (ident,path,_,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",path)] ()
  let imgrels = map toImgRel imgs
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let linkrels = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc = mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")] $ baserels ++ imgrels ++ linkrels
  let relEntry = toEntry "word/_rels/document.xml.rels" epochtime
        $ UTF8.fromStringLazy $ showTopElement' reldoc

  -- create entries for images in word/media/...
  let toImageEntry (_,path,_,_,img) = toEntry ("word/" ++ path) epochtime $ toLazy img
  let imageEntries = map toImageEntry imgs

  -- word/document.xml
  let contentEntry = toEntry "word/document.xml" epochtime $ UTF8.fromStringLazy $ showTopElement' contents

  -- footnotes
  let footnotesEntry = toEntry "word/footnotes.xml" epochtime $ UTF8.fromStringLazy $
        showTopElement' footnotes

  -- footnote rels
  let footnoteRelEntry = toEntry "word/_rels/footnotes.xml.rels" epochtime $ UTF8.fromStringLazy $
        showTopElement' $ mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")]
        $ linkrels

  -- styles
  let newstyles = styleToOpenXml $ writerHighlightStyle opts
  let stylepath = "word/styles.xml"
  styledoc <- parseXml refArchive stylepath
  let styledoc' = styledoc{ elContent = elContent styledoc ++ map Elem newstyles }
  let styleEntry = toEntry stylepath epochtime $ UTF8.fromStringLazy $ showTopElement' styledoc'

  -- construct word/numbering.xml
  let numpath = "word/numbering.xml"
  numEntry <- (toEntry numpath epochtime . UTF8.fromStringLazy . showTopElement')
                 `fmap` mkNumbering (stNumStyles st) (stLists st)
  let docPropsPath = "docProps/core.xml"
  let docProps = mknode "cp:coreProperties"
          [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
          ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
          ,("xmlns:dcterms","http://purl.org/dc/terms/")
          ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
          ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
          $ mknode "dc:title" [] (stringify tit)
          : mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")]
            (maybe "" id $ normalizeDate $ stringify date)
          : mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] () -- put current time here
          : map (mknode "dc:creator" [] . stringify) auths
  let docPropsEntry = toEntry docPropsPath epochtime $ UTF8.fromStringLazy $ showTopElement' docProps
  let relsPath = "_rels/.rels"
  rels <- case findEntryByPath relsPath refArchive of
                   Just e  -> return $ UTF8.toStringLazy $ fromEntry e
                   Nothing -> err 57 "could not find .rels/_rels in reference docx"

  -- fix .rels/_rels, which can get screwed up when reference.docx is edited by Word
  let rels' = substitute "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
                  "http://schemas.openxmlformats.org/officedocument/2006/relationships/metadata/core-properties"
                  rels
  let relsEntry = toEntry relsPath epochtime $ UTF8.fromStringLazy rels'

  -- Create archive
  let archive = foldr addEntryToArchive refArchive $
                  contentTypesEntry : relsEntry : contentEntry : relEntry : footnoteRelEntry : numEntry : styleEntry : footnotesEntry : docPropsEntry : imageEntries
  return $ fromArchive archive

styleToOpenXml :: Style -> [Element]
styleToOpenXml style = parStyle : map toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype = mknode "w:style" [("w:type","character"),
                           ("w:customStyle","1"),("w:styleId",show toktype)]
                             [ mknode "w:name" [("w:val",show toktype)] ()
                             , mknode "w:basedOn" [("w:val","VerbatimChar")] ()
                             , mknode "w:rPr" [] $
                               [ mknode "w:color" [("w:val",tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:shd" [("w:val","clear"),("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ] ++
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ]
                             ]
        tokStyles = tokenStyles style
        tokFeature f toktype = maybe False f $ lookup toktype tokStyles
        tokCol toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenColor =<< lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenBackground =<< lookup toktype tokStyles)
                           `mplus` backgroundColor style
        parStyle = mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               $ mknode "w:wordWrap" [("w:val","off")] ()
                               : ( maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill",drop 1 $ fromColor col)] ()])
                                 $ backgroundColor style )
                             ]

mkNumbering :: M.Map ListMarker Int -> [ListMarker] -> IO Element
mkNumbering markers lists = do
  elts <- mapM mkAbstractNum (M.toList markers)
  return $ mknode "w:numbering"
     [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")]
     $ elts ++ zipWith (mkNum markers) lists [1..(length lists)]

mkNum :: M.Map ListMarker Int -> ListMarker -> Int -> Element
mkNum markers marker numid =
  mknode "w:num" [("w:numId",show numid)]
   $ mknode "w:abstractNumId" [("w:val",show absnumid)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",show (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",show start)] ()) [0..6]
   where absnumid = maybe 0 id $ M.lookup marker markers

mkAbstractNum :: (ListMarker,Int) -> IO Element
mkAbstractNum (marker,numid) = do
  nsid <- randomRIO (0x10000000 :: Integer, 0xFFFFFFFF :: Integer)
  return $ mknode "w:abstractNum" [("w:abstractNumId",show numid)]
    $ mknode "w:nsid" [("w:val", printf "%8x" nsid)] ()
    : mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker) [0..6]

mkLvl :: ListMarker -> Int -> Element
mkLvl marker lvl =
  mknode "w:lvl" [("w:ilvl",show lvl)] $
    [ mknode "w:start" [("w:val",start)] ()
      | marker /= NoMarker && marker /= BulletMarker ] ++
    [ mknode "w:numFmt" [("w:val",fmt)] ()
    , mknode "w:lvlText" [("w:val",lvltxt)] ()
    , mknode "w:lvlJc" [("w:val","left")] ()
    , mknode "w:pPr" []
      [ mknode "w:tabs" []
        $ mknode "w:tab" [("w:val","num"),("w:pos",show $ lvl * step)] ()
      , mknode "w:ind" [("w:left",show $ lvl * step + hang),("w:hanging",show hang)] ()
      ]
    ]
    where (fmt, lvltxt, start) =
            case marker of
                 NoMarker             -> ("bullet"," ","1")
                 BulletMarker         -> ("bullet",bulletFor lvl,"1")
                 NumberMarker st de n -> (styleFor st lvl
                                         ,patternFor de ("%" ++ show (lvl + 1))
                                         ,show n)
          step = 720
          hang = 480
          bulletFor 0 = "\x2022"  -- filled circle
          bulletFor 1 = "\x2013"  -- en dash
          bulletFor 2 = "\x2022"  -- hyphen bullet
          bulletFor 3 = "\x2013"
          bulletFor 4 = "\x2022"
          bulletFor 5 = "\x2013"
          bulletFor _ = "\x2022"
          styleFor UpperAlpha _ = "upperLetter"
          styleFor LowerAlpha _ = "lowerLetter"
          styleFor UpperRoman _ = "upperRoman"
          styleFor LowerRoman _ = "lowerRoman"
          styleFor Decimal _ = "decimal"
          styleFor DefaultStyle 1 = "decimal"
          styleFor DefaultStyle 2 = "lowerLetter"
          styleFor DefaultStyle 3 = "lowerRoman"
          styleFor DefaultStyle 4 = "decimal"
          styleFor DefaultStyle 5 = "lowerLetter"
          styleFor DefaultStyle 6 = "lowerRoman"
          styleFor _ _ = "decimal"
          patternFor OneParen s = s ++ ")"
          patternFor TwoParens s = "(" ++ s ++ ")"
          patternFor _ s = s ++ "."

getNumId :: WS Int
getNumId = length `fmap` gets stLists

-- | Convert Pandoc document to two OpenXML elements (the main document and footnotes).
writeOpenXML :: WriterOptions -> Pandoc -> WS (Element, Element)
writeOpenXML opts (Pandoc (Meta tit auths dat) blocks) = do
  title <- withParaProp (pStyle "Title") $ blocksToOpenXML opts [Para tit | not (null tit)]
  authors <- withParaProp (pStyle "Authors") $ blocksToOpenXML opts
                 [Para (intercalate [LineBreak] auths) | not (null auths)]
  date <- withParaProp (pStyle "Date") $ blocksToOpenXML opts [Para dat | not (null dat)]
  let convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs) = Str (x ++ y) : xs
      convertSpace xs = xs
  let blocks' = bottomUp convertSpace $ blocks
  doc' <- blocksToOpenXML opts blocks'
  notes' <- reverse `fmap` gets stFootnotes
  let meta = title ++ authors ++ date
  let stdAttributes =
            [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
            ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
            ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
            ,("xmlns:o","urn:schemas-microsoft-com:office:office")
            ,("xmlns:v","urn:schemas-microsoft-com:vml")
            ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
            ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
            ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
            ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")]
  let doc = mknode "w:document" stdAttributes $ mknode "w:body" [] (meta ++ doc')
  let notes = mknode "w:footnotes" stdAttributes notes'
  return (doc, notes)

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: WriterOptions -> [Block] -> WS [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

pStyle :: String -> Element
pStyle sty = mknode "w:pStyle" [("w:val",sty)] ()

rStyle :: String -> Element
rStyle sty = mknode "w:rStyle" [("w:val",sty)] ()

getUniqueId :: MonadIO m => m String
-- the + 20 is to ensure that there are no clashes with the rIds
-- already in word/document.xml.rel
getUniqueId = liftIO $ (show . (+ 20) . hashUnique) `fmap` newUnique

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: WriterOptions -> Block -> WS [Element]
blockToOpenXML _ Null = return []
blockToOpenXML opts (Header lev (ident,_,_) lst) = do
  contents <- withParaProp (pStyle $ "Heading" ++ show lev) $
               blockToOpenXML opts (Para lst)
  usedIdents <- gets stSectionIds
  let bookmarkName = if null ident
                        then uniqueIdent lst usedIdents
                        else ident
  modify $ \s -> s{ stSectionIds = bookmarkName : stSectionIds s }
  id' <- getUniqueId
  let bookmarkStart = mknode "w:bookmarkStart" [("w:id", id')
                                               ,("w:name",bookmarkName)] ()
  let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
  return $ [bookmarkStart] ++ contents ++ [bookmarkEnd]
blockToOpenXML opts (Plain lst) = blockToOpenXML opts (Para lst)
-- title beginning with fig: indicates that the image is a figure
blockToOpenXML opts (Para [Image alt (src,'f':'i':'g':':':tit)]) = do
  paraProps <- getParaProps
  contents <- inlinesToOpenXML opts [Image alt (src,tit)]
  captionNode <- withParaProp (pStyle "ImageCaption")
                 $ blockToOpenXML opts (Para alt)
  return $ mknode "w:p" [] (paraProps ++ contents) : captionNode
blockToOpenXML opts (Para lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) = do
    -- chop into several paragraphs so each displaymath is its own
    let lsts = groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
    blocksToOpenXML opts (map Para lsts)
  | otherwise = do
    paraProps <- getParaProps
    contents <- inlinesToOpenXML opts lst
    return [mknode "w:p" [] (paraProps ++ contents)]
blockToOpenXML _ (RawBlock format str)
  | format == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise           = return []
blockToOpenXML opts (BlockQuote blocks) =
  withParaProp (pStyle "BlockQuote") $ blocksToOpenXML opts blocks
blockToOpenXML opts (CodeBlock attrs str) =
  withParaProp (pStyle "SourceCode") $ blockToOpenXML opts $ Para [Code attrs str]
blockToOpenXML _ HorizontalRule = return [
  mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML opts (Table caption aligns widths headers rows) = do
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaProp (pStyle "TableCaption")
                      $ blockToOpenXML opts (Para caption)
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  let cellToOpenXML (al, cell) = withParaProp (alignmentFor al)
                                    $ blocksToOpenXML opts cell
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (\cells -> mapM cellToOpenXML $ zip aligns cells)
           $ rows
  {-let borderProps = mknode "w:tcPr" []-}
                    {-[ mknode "w:tcBorders" []-}
                      {-$ mknode "w:bottom" [("w:val","single")] ()-}
                    {-, mknode "w:vAlign" [("w:val","bottom")] () ]-}
  let borderProps = mknode "w:tcPr" []
                    [ mknode "w:shd" [("w:val", "clear"), ("w:color", "auto"), ("w:fill", "D9D9D9")] ()
                    , mknode "w:vAlign" [("w:val","bottom")] () 
                    ]
  let mkcell border contents = mknode "w:tc" []
                            $ [ borderProps | border ] ++
                            if null contents
                               then [mknode "w:p" [] ()]
                               else contents
  --let mkrow border cells = mknode "w:tr" [] $ map (mkcell border) cells
  let mkrow border cells = mknode "w:tr" []
        ([mknode "w:trPr" [] [ mknode "w:tblHeader" [] () ] | border] 
        ++ [ mknode "w:rPr" [] 
                [
                    mknode "w:rFonts" [("w:ascii", "Arial"), ("w:hAnsi", "Arial")] (),
                    mknode "w:b" [] (),
                    mknode "w:sz" [("w:val", "20")] ()
                ] 
           | border]
        ++ map (mkcell border) cells)
  let textwidth = 7920  -- 5.5 in in twips, 1/20 pt
  let mkgridcol w = mknode "w:gridCol"
                       [("w:w", show $ (floor (textwidth * w) :: Integer))] ()
  return $
    [ mknode "w:tbl" []
      ( mknode "w:tblPr" []
        ( [ mknode "w:tblStyle" [("w:val","TableNormal")] () ] ++
          [ mknode "w:tblCaption" [("w:val", captionStr)] () | not (null caption) ] ++
          [ mknode "w:tblBorders" [] 
                [
                    mknode "w:top"      [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] (),
                    mknode "w:left"     [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] (),
                    mknode "w:bottom"   [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] (),
                    mknode "w:right"    [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] (),
                    mknode "w:insideH"  [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] (),
                    mknode "w:insideV"  [("w:val", "single"), ("w:sz", "4"), ("w:space", "0"), ("w:color", "auto")] ()
                ]
          ] ++
          [mknode "w:tblLook" [("w:val", "0420")] ()]
        )
      : mknode "w:trPr" [] [ mknode "w:cantSplit" [] () ]
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow True headers' | not (all null headers) ] ++
      map (mkrow False) rows'
      )
    ] ++ caption'
blockToOpenXML opts (BulletList lst) = do
  let marker = BulletMarker
  addList marker
  numid  <- getNumId
  asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
blockToOpenXML opts (OrderedList (start, numstyle, numdelim) lst) = do
  let marker = NumberMarker numstyle numdelim start
  addList marker
  numid  <- getNumId
  asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
blockToOpenXML opts (DefinitionList items) =
  concat `fmap` mapM (definitionListItemToOpenXML opts) items

definitionListItemToOpenXML  :: WriterOptions -> ([Inline],[[Block]]) -> WS [Element]
definitionListItemToOpenXML opts (term,defs) = do
  term' <- withParaProp (pStyle "DefinitionTerm")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaProp (pStyle "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

addList :: ListMarker -> WS ()
addList marker = do
  lists <- gets stLists
  modify $ \st -> st{ stLists = lists ++ [marker] }
  numStyles <- gets stNumStyles
  case M.lookup marker numStyles of
           Just _  -> return ()
           Nothing -> modify $ \st ->
                 st{ stNumStyles = M.insert marker (M.size numStyles + 1) numStyles }

listItemToOpenXML :: WriterOptions -> Int -> [Block] -> WS [Element]
listItemToOpenXML _ _ []                   = return []
listItemToOpenXML opts numid (first:rest) = do
  first' <- withNumId numid $ blockToOpenXML opts first
  rest'  <- withNumId 1      $ blocksToOpenXML opts rest
  return $ first' ++ rest'

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: WriterOptions -> [Inline] -> WS [Element]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

withNumId :: Int -> WS a -> WS a
withNumId numid p = do
  origNumId <- gets stListNumId
  modify $ \st -> st{ stListNumId = numid }
  result <- p
  modify $ \st -> st{ stListNumId = origNumId }
  return result

asList :: WS a -> WS a
asList p = do
  origListLevel <- gets stListLevel
  modify $ \st -> st{ stListLevel = stListLevel st + 1 }
  result <- p
  modify $ \st -> st{ stListLevel = origListLevel }
  return result

getTextProps :: WS [Element]
getTextProps = do
  props <- gets stTextProperties
  return $ if null props
              then []
              else [mknode "w:rPr" [] $ props]

pushTextProp :: Element -> WS ()
pushTextProp d = modify $ \s -> s{ stTextProperties = d : stTextProperties s }

popTextProp :: WS ()
popTextProp = modify $ \s -> s{ stTextProperties = drop 1 $ stTextProperties s }

withTextProp :: Element -> WS a -> WS a
withTextProp d p = do
  pushTextProp d
  res <- p
  popTextProp
  return res

getParaProps :: WS [Element]
getParaProps = do
  props <- gets stParaProperties
  listLevel <- gets stListLevel
  numid <- gets stListNumId
  let listPr = if listLevel >= 0
                  then [ mknode "w:numPr" []
                         [ mknode "w:numId" [("w:val",show numid)] ()
                         , mknode "w:ilvl" [("w:val",show listLevel)] () ]
                       ]
                  else []
  return $ case props ++ listPr of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

pushParaProp :: Element -> WS ()
pushParaProp d = modify $ \s -> s{ stParaProperties = d : stParaProperties s }

popParaProp :: WS ()
popParaProp = modify $ \s -> s{ stParaProperties = drop 1 $ stParaProperties s }

withParaProp :: Element -> WS a -> WS a
withParaProp d p = do
  pushParaProp d
  res <- p
  popParaProp
  return res

formattedString :: String -> WS [Element]
formattedString str = do
  props <- getTextProps
  return [ mknode "w:r" [] $
             props ++
             [ mknode "w:t" [("xml:space","preserve")] str ] ]

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: WriterOptions -> Inline -> WS [Element]
inlineToOpenXML _ (Str str) = formattedString str
inlineToOpenXML opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML _ LineBreak = return [br]
inlineToOpenXML _ (RawInline f str)
  | f == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise      = return []
inlineToOpenXML opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML opts (Math mathType str) = do
  let displayType = if mathType == DisplayMath
                       then DisplayBlock
                       else DisplayInline
  case texMathToOMML displayType str of
        Right r -> return [r]
        Left  _ -> inlinesToOpenXML opts (readTeXMath str)
inlineToOpenXML opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML _ (Code attrs str) =
  withTextProp (rStyle "VerbatimChar")
  $ case highlight formatOpenXML attrs str of
         Nothing  -> intercalate [br]
                     `fmap` (mapM formattedString $ lines str)
         Just h   -> return h
     where formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
           toHlTok (toktype,tok) = mknode "w:r" []
                                     [ mknode "w:rPr" []
                                       [ rStyle $ show toktype ]
                                     , mknode "w:t" [("xml:space","preserve")] tok ]
inlineToOpenXML opts (Note bs) = do
  notes <- gets stFootnotes
  notenum <- getUniqueId
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] (rStyle "FootnoteRef")
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline "openxml" $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs
  oldListLevel <- gets stListLevel
  oldParaProperties <- gets stParaProperties
  oldTextProperties <- gets stTextProperties
  modify $ \st -> st{ stListLevel = -1, stParaProperties = [], stTextProperties = [] }
  contents <- withParaProp (pStyle "FootnoteText") $ blocksToOpenXML opts
                $ insertNoteRef bs
  modify $ \st -> st{ stListLevel = oldListLevel, stParaProperties = oldParaProperties,
                      stTextProperties = oldTextProperties }
  let newnote = mknode "w:footnote" [("w:id", notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] (rStyle "FootnoteRef")
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML opts (Link txt ('#':xs,_)) = do
  contents <- withTextProp (rStyle "Link") $ inlinesToOpenXML opts txt
  return [ mknode "w:hyperlink" [("w:anchor",xs)] contents ]
-- external link:
inlineToOpenXML opts (Link txt (src,_)) = do
  contents <- withTextProp (rStyle "Link") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId"++) `fmap` getUniqueId
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",id')] contents ]
inlineToOpenXML opts (Image alt (src, tit)) = do
  -- first, check to see if we've already done this image
  imgs <- gets stImages
  case M.lookup src imgs of
    Just (_,_,_,elt,_) -> return [elt]
    Nothing -> do
      let sourceDir = writerSourceDirectory opts
      res <- liftIO $ E.try $ fetchItem sourceDir src
      case res of
        Left (_ :: E.SomeException) -> do
          liftIO $ warn $ "Could not find image `" ++ src ++ "', skipping..."
          -- emit alt text
          inlinesToOpenXML opts alt
        Right (img, _) -> do
          ident <- ("rId"++) `fmap` getUniqueId
          let size = imageSize img
          let (xpt,ypt) = maybe (120,120) sizeInPoints size
          -- 12700 emu = 1 pt
          let (xemu,yemu) = (xpt * 12700, ypt * 12700)
          let cNvPicPr = mknode "pic:cNvPicPr" [] $
                           mknode "a:picLocks" [("noChangeArrowheads","1"),("noChangeAspect","1")] ()
          let nvPicPr  = mknode "pic:nvPicPr" []
                          [ mknode "pic:cNvPr"
                              [("descr",src),("id","0"),("name","Picture")] ()
                          , cNvPicPr ]
          let blipFill = mknode "pic:blipFill" []
                           [ mknode "a:blip" [("r:embed",ident)] ()
                           , mknode "a:stretch" [] $ mknode "a:fillRect" [] () ]
          let xfrm =    mknode "a:xfrm" []
                          [ mknode "a:off" [("x","0"),("y","0")] ()
                          , mknode "a:ext" [("cx",show xemu),("cy",show yemu)] () ]
          let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                           mknode "a:avLst" [] ()
          let ln =      mknode "a:ln" [("w","9525")]
                          [ mknode "a:noFill" [] ()
                          , mknode "a:headEnd" [] ()
                          , mknode "a:tailEnd" [] () ]
          let spPr =    mknode "pic:spPr" [("bwMode","auto")]
                          [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
          let graphic = mknode "a:graphic" [] $
                          mknode "a:graphicData" [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
                            [ mknode "pic:pic" []
                              [ nvPicPr
                              , blipFill
                              , spPr ] ]
          let imgElt = mknode "w:r" [] $
               mknode "w:drawing" [] $
                 mknode "wp:inline" []
                  [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
                  , mknode "wp:effectExtent" [("b","0"),("l","0"),("r","0"),("t","0")] ()
                  , mknode "wp:docPr" [("descr",tit),("id","1"),("name","Picture")] ()
                  , graphic ]
          let imgext = case imageType img of
                             Just Png  -> ".png"
                             Just Jpeg -> ".jpeg"
                             Just Gif  -> ".gif"
                             Just Pdf  -> ".pdf"
                             Nothing   -> takeExtension src
          if null imgext
             then -- without an extension there is no rule for content type
               inlinesToOpenXML opts alt -- return alt to avoid corrupted docx
             else do
               let imgpath = "media/" ++ ident ++ imgext
               let mbMimeType = getMimeType imgpath
               -- insert mime type to use in constructing [Content_Types].xml
               modify $ \st -> st{ stImages =
                   M.insert src (ident, imgpath, mbMimeType, imgElt, img)
                           $ stImages st }
               return [imgElt]

br :: Element
br = mknode "w:r" [] [mknode "w:cr" [] () ]

parseXml :: Archive -> String -> IO Element
parseXml refArchive relpath =
  case (findEntryByPath relpath refArchive >>= parseXMLDoc . UTF8.toStringLazy . fromEntry) of
       Just d  -> return d
       Nothing -> fail $ relpath ++ " missing in reference docx"

isDisplayMath :: Inline -> Bool
isDisplayMath (Math DisplayMath _) = True
isDisplayMath _                    = False
