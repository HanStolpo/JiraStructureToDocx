-- Blah blah
 {-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts, DeriveGeneric #-}
module ImageStripper where
import Data.Char
import Control.Applicative
import GHC.Generics
import Text.PrettyPrint.GenericPretty as GP
import IssueHierarchy
import DescriptionParser
import qualified Data.Map as M
import Data.List
import JiraTypes
import StrStdTypes

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
        walk (IssueHierarchyRoot cs) = concat . map walk $ cs
        walk IssueHierarchy {ihIssue = i, ihChildren = cs} 
                | null cs  = extractFromIssue i 
                | otherwise = concat $ (extractFromIssue i : map walk cs) 
        -- Get image map for the issue
        extractFromIssue (JsIssue{jsiKey = k, jsiDescription = Just d, jsiAttachments = as}) = extract k d as
        extractFromIssue _ = [] -- doen't have an issue or doesn't have a description
        -- Get the list of images from the description
        extract :: String -> String -> [Attachment] -> [((String, String), Image)]
        extract k d as = map (toImg k as) $ imagesFromDescription $ d 
        -- Convert an ImageLink to an ImageMap entry
        toImg k as ImageLink {imgLink = img} = ((k,img), makeImage img )
            where
                -- If the image link starts with http its a full uri
                makeImage img' | isPrefixOf "http" img' = Image img' img' Nothing
                -- Otherwise see if it is in the list of attachments
                makeImage img' = let imgL = map toLower img' in
                    case find ((imgL ==) . ((map toLower) . attFileName)) as of
                        Just a -> Image img' (attUri a) Nothing      -- It is an attachment return its URI
                        Nothing -> Image img' img' Nothing           -- It is not so return as is



replaceImagesUri :: ImageMap -> IssueHierarchy -> IssueHierarchy
replaceImagesUri im ih = walk ih
    where
        walk ih'@IssueHierarchyRoot{ihChildren = cs} = ih'{ihChildren = map walk cs}
        walk ih'@IssueHierarchy{ihIssue = i, ihChildren = cs} = ih' {ihIssue = newIssue i , ihChildren = map walk cs}
            where
                newIssue i'@(JsIssue {jsiKey = k, jsiDescription = d}) = i'{jsiDescription = replaceImageLinks replacer <$> d}
                    where
                        replacer iml@ImageLink {imgLink = img} = 
                            case M.lookup (k,img) im of
                                Just (Image {imgUrl = url}) -> iml {imgLink = url}
                                Nothing -> iml

extractImagesFromTests :: TestDesc a => [a] -> ImageMap
extractImagesFromTests ts = M.fromList . concatMap (extractFromIssue . tstIssue) $ ts
    where
        -- Get image map for the issue
        extractFromIssue (JsIssue{jsiKey = k, jsiDescription = Just d, jsiAttachments = as}) = extract k d as
        extractFromIssue _ = [] -- doen't have an issue or doesn't have a description
        -- Get the list of images from the description
        extract :: String -> String -> [Attachment] -> [((String, String), Image)]
        extract k d as = map (toImg k as) $ imagesFromDescription $ d 
        -- Convert an ImageLink to an ImageMap entry
        toImg k as ImageLink {imgLink = img} = ((k,img), makeImage img )
            where
                -- If the image link starts with http its a full uri
                makeImage img' | isPrefixOf "http" img' = Image img' img' Nothing
                -- Otherwise see if it is in the list of attachments
                makeImage img' = let imgL = map toLower img' in
                    case find ((imgL ==) . ((map toLower) . attFileName)) as of
                        Just a -> Image img' (attUri a) Nothing      -- It is an attachment return its URI
                        Nothing -> Image img' img' Nothing           -- It is not so return as is



replaceImagesUriInTests :: TestDesc a => ImageMap -> [a] -> [a]
replaceImagesUriInTests im ts = map trans ts
    where
        trans t = tstModifyIssue t . newIssue . tstIssue $ t
        newIssue i'@(JsIssue {jsiKey = k, jsiDescription = d}) = i'{jsiDescription = replaceImageLinks replacer <$> d}
            where
                replacer iml@ImageLink {imgLink = img} = 
                    case M.lookup (k,img) im of
                        Just (Image {imgUrl = url}) -> iml {imgLink = url}
                        Nothing -> iml
