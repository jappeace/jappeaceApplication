{-# LANGUAGE OverloadedStrings #-}

-- | Asserts that the webwinkelverhuis.nl links on the jappiesoftware.com
-- pages follow the 'WebwinkelverhuisUrl' passed to the page functions: serve
-- mode must cross-link to the locally hosted copy, production to the live
-- domain. Each page is rendered with a fake origin and fails if the fake
-- origin is missing or the live domain is still hardcoded in a href.
module Main (main) where

-- Decision: tasty + tasty-hunit as the test framework, suite named "unit",
-- following the haskell-template-project convention used across the other
-- repositories (mijn-webwinkel-migraine's suite has the same shape).
-- Alternatives considered: a hand-rolled exitcode-stdio main (zero extra
-- dependencies, but no per-case output and off-convention; this suite's
-- first version was written that way and ported) and hspec (no house
-- precedent).

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import PenguinTemplates
  ( WebwinkelverhuisUrl(..)
  , penguinIndexPage
  , penguinIndexPageNl
  , penguinWordpressPage
  , penguinWordpressPageNl
  )
import WebwinkelTemplates
  ( webwinkelIndexPage
  , mijnwebwinkelMigrationPage
  , ccvshopMigrationPage
  , lightspeedMigrationPage
  , mijnwebwinkelWaaromPage
  , lightspeedWaaromPage
  )

-- | A recognisable fake origin: it can only appear in the output when the
-- page honours the parameter instead of a hardcoded domain.
testOrigin :: Text
testOrigin = "http://origin-under-test.example"

-- | Every jappiesoftware.com page that links to the webwinkel site, named
-- for test output.
pagesUnderTest :: [(String, WebwinkelverhuisUrl -> Html)]
pagesUnderTest =
  [ ("penguinIndexPage", penguinIndexPage)
  , ("penguinIndexPageNl", penguinIndexPageNl)
  , ("penguinWordpressPage", penguinWordpressPage)
  , ("penguinWordpressPageNl", penguinWordpressPageNl)
  ]

-- | Render the page with the fake origin and assert the links follow it.
-- The live domain may still appear as visible text (the brand name is
-- spelled out on the pages), so only href occurrences count.
linksFollowOriginCase :: (String, WebwinkelverhuisUrl -> Html) -> TestTree
linksFollowOriginCase (pageName, page) = testCase pageName $ do
  let rendered = TL.toStrict (renderHtml (page (WebwinkelverhuisUrl testOrigin)))
  assertBool ("expected a link to " <> T.unpack testOrigin <> " but found none")
    (("href=\"" <> testOrigin <> "/\"") `T.isInfixOf` rendered)
  assertBool "links the hardcoded live domain instead of the passed origin"
    (not ("href=\"https://webwinkelverhuis.nl" `T.isInfixOf` rendered))

-- | Every webwinkelverhuis.nl page with a "plan een gesprek" button, named
-- for test output.
meetLinkPages :: [(String, Html)]
meetLinkPages =
  [ ("webwinkelIndexPage", webwinkelIndexPage)
  , ("mijnwebwinkelMigrationPage", mijnwebwinkelMigrationPage)
  , ("ccvshopMigrationPage", ccvshopMigrationPage)
  , ("lightspeedMigrationPage", lightspeedMigrationPage)
  , ("mijnwebwinkelWaaromPage", mijnwebwinkelWaaromPage)
  , ("lightspeedWaaromPage", lightspeedWaaromPage)
  , ("penguinWordpressPage", penguinWordpressPage (WebwinkelverhuisUrl testOrigin))
  , ("penguinWordpressPageNl", penguinWordpressPageNl (WebwinkelverhuisUrl testOrigin))
  ]

-- | Scheduling buttons must use the branded meet.jappiesoftware.com redirect,
-- never a raw calendar.app.google URL: the raw link routed to the wrong
-- calendar once, and the redirect is the single place the target may change.
usesMeetLinkCase :: (String, Html) -> TestTree
usesMeetLinkCase (pageName, page) = testCase pageName $ do
  let rendered = TL.toStrict (renderHtml page)
  assertBool "expected a link to https://meet.jappiesoftware.com but found none"
    ("href=\"https://meet.jappiesoftware.com\"" `T.isInfixOf` rendered)
  assertBool "links a raw calendar.app.google URL instead of the meet redirect"
    (not ("calendar.app.google" `T.isInfixOf` rendered))

main :: IO ()
main = defaultMain $
  testGroup "shake-blog templates"
    [ testGroup "webwinkel links follow WebwinkelverhuisUrl"
        (map linksFollowOriginCase pagesUnderTest)
    , testGroup "scheduling buttons use meet.jappiesoftware.com"
        (map usesMeetLinkCase meetLinkPages)
    ]
