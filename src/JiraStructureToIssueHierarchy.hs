-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric, TupleSections, DoAndIfThenElse#-}
import Network.HTTP.Conduit
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BW8
import Network (withSocketsDo)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import Text.JSON as JS
import Text.JSON.String as JS
import Text.JSON.Pretty as JS
import Data.Aeson as AS
import Data.Char
import Data.Typeable
import Data.Data
import Control.Monad.Error
import GHC.Generics
import Control.Exception
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Attoparsec.Combinator as AP
import Debug.Trace
import Text.PrettyPrint.GenericPretty as GP
import qualified Data.Map as M
import Data.List
import System.Directory
import System.Environment
-- local files
import IssueHierarchy
import DescriptionParser
import ImageStripper

 
main = do
    args <- getArgs
    if length args /= 4 then
        error "expected: username password baseUrl structureId"
    else
        withSocketsDo $ do
        runResourceT $ do
            manager <- liftIO $ newManager def 
            let usrn = (B.pack $ args!!0)
            let pwd = (B.pack $ args!!1)
            let baseUrl = args!!2
            let structureId = args!!3
            let url =  baseUrl ++ "/rest/structure/1.0/structure/" ++ structureId ++"/forest"
            let req = applyBasicAuth  usrn pwd $ (fromJust $ parseUrl url)
            res <- httpLbs req manager
            forest <- decodeForest $ responseBody res
            hierarchy <- forestToHierarchy manager usrn pwd (baseUrl ++ "/rest/api/2/issue/") forest
            let images = extractImages hierarchy
            imagesLoc <- localizeImages manager usrn pwd baseUrl images
            let hierarchy' = replaceImagesUri imagesLoc hierarchy
            cd <- liftIO $ getCurrentDirectory
            liftIO $ writeFile (cd ++ "/IssueHierarchy.txt") $ GP.pretty hierarchy'
            return ()

---------------------------------------------------------------------------
-- The JASON representation of a fores
data JsForest = JsForest
    {
        jsfStructure :: Int,           -- The structure ID
        jsfVersion :: Int,             -- The version
        jsFroot :: Maybe Int,          -- The optional root ID
        jsfFormula :: BW8.ByteString   -- The structure forest formula
    } 
    deriving (Data, Typeable, Show, Generic)

-- Specify JSON parser for JsForest
instance FromJSON JsForest where
    parseJSON (AS.Object v) =   JsForest <$>
                                v AS..: "structure" <*>
                                v AS..: "version" <*>
                                v AS..:? "root" <*>
                                v AS..: "formula"

-- Get JsForest given query resutl
decodeJsForest :: Monad m => L.ByteString -> m JsForest
decodeJsForest s = 
    case r of
        Left e -> fail e
        Right f -> return f
    where
        r = AS.eitherDecode s
    


---------------------------------------------------------------------------
-- Intermediate representation of a forest ie structure 
data Forest = Forest 
    { 
        ftIssueId :: Maybe Int,     -- The possible issue ID which is Nothing for root
        ftChildren :: [Forest]      -- The children at this level in the hierarchy
    }
    deriving (Data, Typeable, Show, Generic)

instance Out Forest

-- Decode the forest structure from the query
decodeForest :: Monad m => L.ByteString -> m Forest
decodeForest s = do
      jsForest <- decodeJsForest s
      listIdDepth <- decodeForestFormula $ jsfFormula jsForest
      return $ buildForest listIdDepth
 
-- Decode the forest formula into list of (issueId, issueDepth) pairs
decodeForestFormula :: Monad m => BW8.ByteString -> m [(Int, Int)]
decodeForestFormula s = 
    case (r :: Either String [(Int, Int)]) of 
        Left e -> fail e
        Right l -> return l
    where
        r = AP.parseOnly p s
        p = pTpl `AP.sepBy` (AP.string ",") 
        pTpl = makeTpl <$> AP.number <* AP.string ":" <*> AP.number
        makeTpl (AP.I a) (AP.I b) = (fromIntegral a, fromIntegral b)

-- Build the Forest hierarchy given the decoded forest formula as list of (issueId, issueDepth) pairs
buildForest :: [(Int,Int)] -> Forest
buildForest listIdDepth = Forest Nothing (fst $ makeC (-1) listIdDepth)
    where
        makeC _ [] = ([], [])
        makeC d allts@((i,d'):ts)
            | d' > d =  
                let 
                    (cs, ts') = makeC d' ts
                    (ss, ts'') = makeS d' ts'
                in (Forest (Just i) cs : ss, ts'')
            | otherwise = ([], allts)
        makeS d [] = ([], [])
        makeS d allts@((i,d'):ts)
            | d' == d = 
                let
                    (cs, ts') = makeC d' ts
                    (ss, ts'') = makeS d' ts'
                in (Forest (Just i) cs : ss, ts'')
            | otherwise = ([], allts)
                    
                

---------------------------------------------------------------------------
-- Class to represent the JASON of an issue

data JsIssue = JsIssue
    {
        jsiKey :: String,               -- The issue key
        jsiSummary :: String,           -- The summary field 
        jsiDescription :: Maybe String, -- The optional description field
        jsiAttachments :: [Attachment]  -- The list of optional attachments
    } 
    deriving (Show, Generic)

-- Specify JSON parser for JsIssue 
instance FromJSON JsIssue where
    parseJSON (AS.Object v) = do
        key <- v AS..: "key"
        fields <- v AS..: "fields"
        summary <- fields AS..: "summary"
        description <- fields AS..:? "description"
        attachments <- fields AS..:? "attachment" AS..!= []
        return $ JsIssue key summary description attachments

decodeJsIssue :: Monad m => L.ByteString -> m JsIssue
decodeJsIssue s = 
    case r of
        Left e -> fail e
        Right f -> return f
    where
        r = AS.eitherDecode s

---------------------------------------------------------------------------
-- 
forestToHierarchy manager usrn pwd baseUrl forest = do
    let issueId = ftIssueId forest
    case issueId of
        Just i -> forestToHierarchyIssue manager usrn pwd baseUrl forest i
        _ -> forestToHierarchyNoIssue manager usrn pwd baseUrl forest
   
forestToHierarchyIssue manager usrn pwd baseUrl forest i = do
    let url = baseUrl ++ (show i) ++ "/?fields=summary,description,attachment"
    let req =  applyBasicAuth  usrn pwd $ (fromJust $ parseUrl url)
    res <- httpLbs req manager
    jsIssue <- decodeJsIssue $ responseBody res
    children <- makeChildren manager usrn pwd baseUrl $ ftChildren forest
    return $ IssueHierarchy (jsiKey jsIssue) (jsiSummary jsIssue) (maybe "" id $ jsiDescription jsIssue) (jsiAttachments jsIssue) children

forestToHierarchyNoIssue manager usrn pwd baseUrl forest = do
    children <- makeChildren manager usrn pwd baseUrl $ ftChildren forest
    return $ IssueHierarchy "root" "" "" [] children

makeChildren manager usrn pwd baseUrl [] =  do return []

makeChildren manager usrn pwd baseUrl (f:fs) =  do
    i <- forestToHierarchy manager usrn pwd baseUrl f
    is <- makeChildren manager usrn pwd baseUrl fs
    return $ i : is

localizeImages :: (Monad m, MonadIO m, MonadBaseControl IO m, MonadResource m) => Manager -> B.ByteString -> B.ByteString -> String -> ImageMap -> m ImageMap
localizeImages manager usrn pwd baseurl im = do
    liftIO $ createDirectoryIfMissing True "./Images"
    liftM M.fromList $ forM (M.toList im) getImg 
        where
            getImg (k, img@(Image origUrl url _))
                | isPrefixOf baseurl url =  liftM (k,) $ getImage manager usrn pwd img
                | otherwise = return (k, img)


getImage manager usrn pwd img@(Image origUrl url _) = do
    let req = applyBasicAuth usrn pwd $ (fromJust $ parseUrl url)
    res <- http req manager
    (responseBody res) C.$$+- sinkFile ("./Images/" ++ fn)
    return $ trace (GP.pretty (img, fn)) $ img {imgUrl = ("./Images/" ++ fn)}
    where 
        fn = reverse . (takeWhile cnd) . reverse $ url
        cnd '/' = False
        cnd _ = True

    
