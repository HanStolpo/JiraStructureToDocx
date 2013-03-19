-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
module ImageStripper where
import Network.HTTP.Conduit
import Data.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BW8
import Network (withSocketsDo)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Conduit.Binary (sinkFile)
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
import Data.Attoparsec.ByteString.Char8 as AP
import Data.Attoparsec.Combinator as AP
import Debug.Trace
import Text.PrettyPrint.GenericPretty as GP
import System.Directory
import IssueHierarchy
import DescriptionParser
import qualified Data.Map as M
import Data.List

-----------------------------------------------------------------------------
--
data Image = Image 
    {
        imgOrigUrl  :: String,       --- The original URL in the description
        imgUrl      :: String,       --- The actual URL possibly corrected form of imgOrigUrl
        imgLocUrl   :: Maybe String  --- If it was possible to localise the image then path to localized location
    }deriving (Show, Generic)

instance Out Image

type ImageMap = M.Map (String,String) Image

extractImages :: IssueHierarchy -> ImageMap
extractImages ih = M.fromList $ walk ih
    where
        -- Traverse the hierarchy
        walk :: IssueHierarchy -> [((String, String), Image)]
        walk IssueHierarchy 
                {   ihKey = k, 
                    ihDescription = d, 
                    ihAttachment = as, 
                    ihChildren = cs
                } 
                | null cs  = extract k d as 
                | otherwise = concat $ (extract  k d as : walk (head cs) : map walk (tail cs)) 
        -- Get the list of images from the description
        extract :: String -> String -> [Attachment] -> [((String, String), Image)]
        extract k d as = map (toImg k as) $ imagesFromDescription $ d 
        -- Convert an ImageLink to an ImageMap entry
        toImg k as ImageLink {imgLink = img} = ((k,img), makeImage img )
            where
                -- If the image link starts with http its a full uri
                makeImage img | isPrefixOf "http" img = Image img img Nothing
                -- Otherwise see if it is in the list of attachments
                makeImage img = let imgL = map toLower img in
                    case find ((imgL ==) . ((map toLower) . attFileName)) as of
                        Just a -> Image img (attUri a) Nothing      -- It is an attachment return its URI
                        Nothing -> Image img img Nothing            -- It is not so return as is



replaceImagesUri :: ImageMap -> IssueHierarchy -> IssueHierarchy
replaceImagesUri im ih = walk ih
    where
        walk ih'@IssueHierarchy
            {
                ihKey = k, 
                ihDescription = d, 
                ihChildren = cs
            } 
            = ih' {ihDescription = replaceImageLinks replacer d, ihChildren = map walk cs}
            where
                replacer imgLink@ImageLink {imgLink = img} = 
                    case M.lookup (k,img) im of
                        Just (Image {imgUrl = url}) -> imgLink {imgLink = url}
                        Nothing -> imgLink

replaceImages :: ImageMap -> IssueHierarchy -> IssueHierarchy
replaceImages = undefined
