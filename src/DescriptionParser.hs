 -- GHC_STATIC_OPTION_i=../src:../testsuite
module DescriptionParser    ( D.parseDescription
                            , D.imagesFromDescription
                            , D.ImageLink(..)
                            , D.replaceImageLinks
                            ) where

import DescriptionParserInner as D
