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
import Data.Time (UTCTime(..), fromGregorian)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import AssetHash (GehashteAssets(..), gehashteAssetNaam, herschrijfAssetVerwijzingen)
import Data.Char (isHexDigit)
import qualified Data.ByteString.Char8 as BSC
import PageChrome (faqAnswerHtml, faqPageJsonLd, renderFaqItem)
import Templates (renderTagPage, renderIndexPage)
import Types (Article(..), PaginationInfo(..), defaultSiteConfig)
import PenguinTemplates
  ( WebwinkelverhuisUrl(..)
  , penguinIndexPage
  , penguinIndexPageNl
  , penguinWordpressPage
  , penguinWordpressPageNl
  )
import WebwinkelTemplates
  ( webwinkelIndexPage
  , scanPage
  , mijnwebwinkelMigrationPage
  , ccvshopMigrationPage
  , lightspeedMigrationPage
  , mijnwebwinkelWaaromPage
  , lightspeedWaaromPage
  , overOnsPage
  , contactPage
  , relativizeWebwinkelContentImages
  , webwinkelverhuisSitemap
  , webwinkelverhuisStaticPages
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
  , ("overOnsPage", overOnsPage)
  , ("contactPage", contactPage)
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

-- | Every statically renderable page of both brand sites: the index pages
-- plus everything already in 'meetLinkPages' (which contains the wordpress
-- and webwinkel service pages).
-- scanPage is listed here but not in 'meetLinkPages': its plan-een-gesprek
-- buttons live inside the Elm scanner app, not in the static HTML.
allStaticPages :: [(String, Html)]
allStaticPages =
  [ ("penguinIndexPage", penguinIndexPage (WebwinkelverhuisUrl testOrigin))
  , ("penguinIndexPageNl", penguinIndexPageNl (WebwinkelverhuisUrl testOrigin))
  , ("scanPage", scanPage)
  ] <> meetLinkPages

-- | No page may publish share-URL tracking parameters. URLs copied from a
-- platform's share button (LinkedIn in particular) embed utm_* junk and an
-- rcm token that identifies the account the link was copied from; pasting
-- one verbatim into a template leaks that token to every visitor. Links
-- must be cleaned before they go in (see voedzameKostAnnouncementUrl).
noTrackingParamsCase :: (String, Html) -> TestTree
noTrackingParamsCase (pageName, page) = testCase pageName $ do
  let rendered = TL.toStrict (renderHtml page)
  assertBool "a link carries share-URL tracking parameters (utm_* or rcm=)"
    (not ("utm_" `T.isInfixOf` rendered) && not ("rcm=" `T.isInfixOf` rendered))

-- | Content images referenced by absolute production URL must come out
-- root-relative so they load on the local serve preview too, while hrefs
-- (canonical link tags, cross-links) must keep their absolute URL.
relativizesImageSourcesOnly :: TestTree
relativizesImageSourcesOnly = testCase "relativizeWebwinkelContentImages" $ do
  let rendered = TL.pack
        "<link href=\"https://webwinkelverhuis.nl/blog/x.html\" rel=\"canonical\">\
        \<img src=\"https://webwinkelverhuis.nl/panzer-shop-shopify-na.png\">"
      rewritten = relativizeWebwinkelContentImages rendered
  assertBool "image src is not root-relative"
    (TL.pack "src=\"/panzer-shop-shopify-na.png\"" `TL.isInfixOf` rewritten)
  assertBool "canonical href lost its absolute URL"
    (TL.pack "href=\"https://webwinkelverhuis.nl/blog/x.html\"" `TL.isInfixOf` rewritten)

-- | FAQ answers are 'Html' so they can carry links; one entry must feed both
-- surfaces: the visible @<dd>@ keeps a working anchor, and the JSON-LD script
-- carries the same link with its markup escaped, so an answer's @<@ can never
-- terminate the surrounding @<script>@ element.
faqAnswersCarryLinks :: TestTree
faqAnswersCarryLinks = testCase "linked answer reaches dd and JSON-LD" $ do
  let entry =
        ( "Testvraag?"
        , faqAnswerHtml $ do
            "zie "
            H.a ! A.href "https://example.com/prijzen" $ "de prijzen"
        )
      renderedItem = TL.toStrict (renderHtml (renderFaqItem entry))
      renderedJsonLd = TL.toStrict (renderHtml (faqPageJsonLd [entry]))
  assertBool "visible dd lost the answer's anchor"
    ("<a href=\"https://example.com/prijzen\">" `T.isInfixOf` renderedItem)
  assertBool "JSON-LD lost the answer's link"
    ("https://example.com/prijzen" `T.isInfixOf` renderedJsonLd)
  assertBool "JSON-LD carries a raw <a; '<' must be escaped inside the script element"
    (not ("<a " `T.isInfixOf` snd (T.breakOn "ld+json" renderedJsonLd)))

main :: IO ()
main = defaultMain $
  testGroup "shake-blog templates"
    [ testGroup "webwinkel links follow WebwinkelverhuisUrl"
        (map linksFollowOriginCase pagesUnderTest)
    , testGroup "scheduling buttons use meet.jappiesoftware.com"
        (map usesMeetLinkCase meetLinkPages)
    , testGroup "no page publishes share-URL tracking parameters"
        (map noTrackingParamsCase allStaticPages)
    , testGroup "webwinkel content images load on the serve preview"
        [relativizesImageSourcesOnly]
    , testGroup "FAQ answers carry markup into both surfaces"
        [faqAnswersCarryLinks]
    , testGroup "webwinkelverhuis sitemap advertises lastmod"
        [ everyStaticPageEntryIsDated
        , blogIndexAdvertisesNewestArticleLastmod
        , articleWithoutModifiedFallsBackToPublicationDate
        ]
    , testGroup "asset cache-busting"
        [ assetVerwijzingenWordenHerschreven
        , gehashteNaamVolgtInhoud
        ]
    , testGroup "taxonomiepagina's zijn noindex"
        [ taxonomieKrijgtNoindexIndexNiet
        ]
    ]

-- | De dunne taxonomiepagina's (tags/categorieen) dragen "noindex, follow";
-- de indexpagina moet gewoon indexeerbaar blijven. Dit faalt zowel wanneer
-- de directive van de tagpagina verdwijnt als wanneer hij per ongeluk op
-- alle pagina's belandt.
taxonomieKrijgtNoindexIndexNiet :: TestTree
taxonomieKrijgtNoindexIndexNiet = testCase "tagpagina noindex, indexpagina niet" $ do
  let tagPagina = TL.toStrict (renderHtml
        (renderTagPage defaultSiteConfig [] Nothing "haskell" []))
      indexPagina = TL.toStrict (renderHtml
        (renderIndexPage defaultSiteConfig [] Nothing []
          (PaginationInfo 1 1 Nothing Nothing)))
  assertBool "tagpagina mist de noindex, follow-directive"
    ("<meta name=\"robots\" content=\"noindex, follow\">" `T.isInfixOf` tagPagina)
  assertBool "indexpagina kreeg onbedoeld een robots-directive"
    (not ("name=\"robots\"" `T.isInfixOf` indexPagina))

-- | Een herkenbare vaste set gehashte namen voor de herschrijftest.
testGehashteAssets :: GehashteAssets
testGehashteAssets = GehashteAssets
  { gehashteStyleCss = "style-aaaaaaaaaa.css"
  , gehashteBlogCss = "blog-bbbbbbbbbb.css"
  , gehashtePrijsCalculatorJs = "prijs-calculator-cccccccccc.js"
  , gehashteScannerFormJs = "scanner-form-dddddddddd.js"
  }

-- | Elke logische assetverwijzing in een pagina moet naar zijn gehashte
-- naam herschreven worden; andere paden moeten met rust gelaten worden.
assetVerwijzingenWordenHerschreven :: TestTree
assetVerwijzingenWordenHerschreven = testCase "logische namen worden gehashte namen" $ do
  let pagina = TL.pack
        "<link href=\"/style.css\"><link href=\"/blog.css\">\
        \<script src=\"/prijs-calculator.js\"></script>\
        \<script src=\"/scanner-form.js\"></script>\
        \<img src=\"/assets/beeld/logo-breed.png\">"
      herschreven = herschrijfAssetVerwijzingen testGehashteAssets pagina
  assertBool "style.css is niet herschreven"
    (TL.pack "href=\"/style-aaaaaaaaaa.css\"" `TL.isInfixOf` herschreven)
  assertBool "blog.css is niet herschreven"
    (TL.pack "href=\"/blog-bbbbbbbbbb.css\"" `TL.isInfixOf` herschreven)
  assertBool "prijs-calculator.js is niet herschreven"
    (TL.pack "src=\"/prijs-calculator-cccccccccc.js\"" `TL.isInfixOf` herschreven)
  assertBool "scanner-form.js is niet herschreven"
    (TL.pack "src=\"/scanner-form-dddddddddd.js\"" `TL.isInfixOf` herschreven)
  assertBool "een niet-gehasht pad is aangeraakt"
    (TL.pack "src=\"/assets/beeld/logo-breed.png\"" `TL.isInfixOf` herschreven)
  assertBool "er verwijst nog een pagina naar de ongehashte stylesheet"
    (not (TL.pack "\"/style.css\"" `TL.isInfixOf` herschreven))

-- | De gehashte naam is deterministisch voor dezelfde inhoud, verandert met
-- de inhoud mee, en volgt het patroon basisnaam-<10 hex>.extensie.
gehashteNaamVolgtInhoud :: TestTree
gehashteNaamVolgtInhoud = testCase "gehashte naam volgt de inhoud" $ do
  let naamA = gehashteAssetNaam "style" "css" (BSC.pack "body { color: red }")
      naamA' = gehashteAssetNaam "style" "css" (BSC.pack "body { color: red }")
      naamB = gehashteAssetNaam "style" "css" (BSC.pack "body { color: blue }")
      hashDeel = take 10 (drop (length ("style-" :: String)) naamA)
  assertBool "zelfde inhoud geeft niet dezelfde naam" (naamA == naamA')
  assertBool "andere inhoud geeft niet een andere naam" (naamA /= naamB)
  assertBool ("naam volgt het patroon niet: " <> naamA)
    (take 6 naamA == "style-"
      && length naamA == length ("style-" :: String) + 10 + length (".css" :: String)
      && all isHexDigit hashDeel
      && drop (length naamA - 4) naamA == ".css")

-- | An article carrying only the fields the sitemap reads; the rest is
-- inert filler.
sitemapTestArticle :: Text -> UTCTime -> Maybe UTCTime -> Article
sitemapTestArticle url published modified = Article
  { articleTitle       = "test"
  , articleSlug        = url
  , articleCategory    = "test"
  , articleDate        = published
  , articleModified    = modified
  , articleTags        = []
  , articleContent     = mempty
  , articleContentText = ""
  , articleSummary     = Nothing
  , articleSummaryText = Nothing
  , articleFootnotesHtml = Nothing
  , articleUrl         = url
  }

-- | The sitemap rendered from two articles: an old post that was
-- recently edited (modified 2026-08-01) and a newer post never edited.
sitemapUnderTest :: Text
sitemapUnderTest = webwinkelverhuisSitemap
  [ sitemapTestArticle "oud-maar-bijgewerkt.html"
      (UTCTime (fromGregorian 2026 6 14) 0)
      (Just (UTCTime (fromGregorian 2026 8 1) 0))
  , sitemapTestArticle "nieuw-nooit-bijgewerkt.html"
      (UTCTime (fromGregorian 2026 7 10) 0)
      Nothing
  ]

everyStaticPageEntryIsDated :: TestTree
everyStaticPageEntryIsDated = testCase "every static page entry carries a lastmod" $
  mapM_
    (\(url, _day) -> assertBool (T.unpack url <> " entry has no lastmod")
      (("<loc>" <> url <> "</loc><lastmod>") `T.isInfixOf` sitemapUnderTest))
    webwinkelverhuisStaticPages

-- | The blog index date must be the newest lastmod over all articles,
-- which here comes from a *modified* override on the oldest post: this
-- fails both when the index ignores modified dates and when it picks
-- the newest publication date instead of the maximum lastmod.
blogIndexAdvertisesNewestArticleLastmod :: TestTree
blogIndexAdvertisesNewestArticleLastmod = testCase "blog index carries the newest article lastmod" $
  assertBool "blog/ entry does not advertise 2026-08-01"
    ("<loc>https://webwinkelverhuis.nl/blog/</loc><lastmod>2026-08-01</lastmod>"
      `T.isInfixOf` sitemapUnderTest)

articleWithoutModifiedFallsBackToPublicationDate :: TestTree
articleWithoutModifiedFallsBackToPublicationDate = testCase "article without modified uses its publication date" $
  assertBool "never-edited article does not carry its publication date"
    ("<loc>https://webwinkelverhuis.nl/blog/nieuw-nooit-bijgewerkt.html</loc><lastmod>2026-07-10</lastmod>"
      `T.isInfixOf` sitemapUnderTest)
