 -- GHC_STATIC_OPTION_i=../src:../testsuite
module Pandoc.Reader.Jira   ( D.parseDescription
                            , D.imagesFromDescription
                            , D.ImageLink(..)
                            , D.replaceImageLinks
                            ) where

import Pandoc.Reader.Jira.Details as D
