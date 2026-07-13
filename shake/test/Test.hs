{-# LANGUAGE OverloadedStrings #-}

-- | Asserts that the webwinkelverhuis.nl links on the jappiesoftware.com
-- pages follow the 'WebwinkelverhuisUrl' passed to the page functions: serve
-- mode must cross-link to the locally hosted copy, production to the live
-- domain. The test renders each page with a fake origin and fails if the
-- fake origin is missing or the live domain is still hardcoded in a href.
module Main (main) where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Exit (exitFailure)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import PenguinTemplates
  ( WebwinkelverhuisUrl(..)
  , penguinIndexPage
  , penguinIndexPageNl
  , penguinWordpressPage
  , penguinWordpressPageNl
  )

-- | A recognisable fake origin: it can only appear in the output when the
-- page honours the parameter instead of a hardcoded domain.
testOrigin :: Text
testOrigin = "http://origin-under-test.example"

-- | Every jappiesoftware.com page that links to the webwinkel site, named
-- for error output.
pagesUnderTest :: [(String, WebwinkelverhuisUrl -> Html)]
pagesUnderTest =
  [ ("penguinIndexPage", penguinIndexPage)
  , ("penguinIndexPageNl", penguinIndexPageNl)
  , ("penguinWordpressPage", penguinWordpressPage)
  , ("penguinWordpressPageNl", penguinWordpressPageNl)
  ]

-- | Render the page with the fake origin and fail loudly unless the links
-- follow it. The live domain may still appear as visible text (the brand
-- name is spelled out on the pages), so only href occurrences count.
assertLinksFollowOrigin :: (String, WebwinkelverhuisUrl -> Html) -> IO ()
assertLinksFollowOrigin (pageName, page) = do
  let rendered = TL.toStrict (renderHtml (page (WebwinkelverhuisUrl testOrigin)))
  unless (("href=\"" <> testOrigin <> "/\"") `T.isInfixOf` rendered) $ do
    putStrLn (pageName <> ": expected a link to " <> T.unpack testOrigin <> " but found none")
    exitFailure
  when ("href=\"https://webwinkelverhuis.nl" `T.isInfixOf` rendered) $ do
    putStrLn (pageName <> ": links the hardcoded live domain instead of the passed origin")
    exitFailure

main :: IO ()
main = do
  mapM_ assertLinksFollowOrigin pagesUnderTest
  putStrLn "webwinkel links follow WebwinkelverhuisUrl on all 4 pages"
