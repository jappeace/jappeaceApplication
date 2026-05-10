{-# LANGUAGE OverloadedStrings #-}
module PenguinTemplates
  ( penguinIndexPage
  , penguinBlogIndexPage
  , penguinArticlePage
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (SiteConfig(..), Article(..), PaginationInfo(..))

-- Helper to convert Text to AttributeValue
toValue :: Text -> H.AttributeValue
toValue = H.toValue

toHtml :: Text -> Html
toHtml = H.toHtml

-- | Custom HTML attribute helper
customAttribute :: Text -> Text -> H.Attribute
customAttribute name val = H.customAttribute (H.textTag name) (toValue val)

-- =============================================================================
-- Base template shared by all penguin pages
-- =============================================================================

penguinBaseTemplate :: Text -> Html -> Html
penguinBaseTemplate title content =
  H.docTypeHtml ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content "Fractional CTO for startups. Technical leadership for non-technical founders."
      H.link ! A.rel "stylesheet" ! A.href "style.css"
      H.link ! A.rel "stylesheet" ! A.href "blog.css"
      H.link ! A.rel "icon" ! A.href "favicon.ico"
      H.script ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.title (toHtml title)
    H.body $ do
      H.header $
        H.nav ! A.class_ "top-nav" $ do
          H.span ! A.class_ "logo" $ "Jappie Software B.V."
          H.ul $ do
            H.li $ H.a ! A.href "#services" $ "Services"
            H.li $ H.a ! A.href "#results" $ "Results"
            H.li $ H.a ! A.href "#approach" $ "Approach"
            H.li $ H.a ! A.href "/blog/" $ "Blog"
            H.li $ H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-link" $ "Get in touch"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href "mailto:hi@jappie.me" $ "hi@jappie.me"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- | Blog-specific base template (uses root-relative paths for assets)
penguinBlogBaseTemplate :: Text -> Html -> Html
penguinBlogBaseTemplate title content =
  H.docTypeHtml ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.link ! A.rel "stylesheet" ! A.href "/style.css"
      H.link ! A.rel "stylesheet" ! A.href "/blog.css"
      H.link ! A.rel "icon" ! A.href "/favicon.ico"
      H.link ! A.href "/blog/atom"
             ! A.type_ "application/atom+xml"
             ! A.rel "alternate"
             ! A.title "Jappie Software B.V. Atom Feed"
      H.script ! A.src "https://d3js.org/d3.v7.min.js" $ mempty
      H.title (toHtml title)
    H.body $ do
      H.header $
        H.nav ! A.class_ "top-nav" $ do
          H.span ! A.class_ "logo" $
            H.a ! A.href "/" $ "Jappie Software B.V."
          H.ul $ do
            H.li $ H.a ! A.href "/#services" $ "Services"
            H.li $ H.a ! A.href "/#results" $ "Results"
            H.li $ H.a ! A.href "/#approach" $ "Approach"
            H.li $ H.a ! A.href "/blog/" $ "Blog"
            H.li $ H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-link" $ "Get in touch"
      content
      H.footer $ do
        H.p $ do
          H.a ! A.href "mailto:hi@jappie.me" $ "hi@jappie.me"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "tel:+31644237437" $ "+31 6 4423 7437"
          H.preEscapedToHtml (" &middot; " :: Text)
          H.a ! A.href "/blog/" $ "Blog"
        H.p $ H.small $ H.preEscapedToHtml ("Jappie Software B.V. &middot; KVK: 95097872" :: Text)
      H.preEscapedToHtml ("<svg class=\"voronoi\"></svg>" :: Text)
      H.script $ H.preEscapedToHtml voronoiScript

-- =============================================================================
-- Landing page (index.html)
-- =============================================================================

penguinIndexPage :: Html
penguinIndexPage = penguinBaseTemplate "Jappie Software B.V. \8212 Fractional CTO for Startups" $
  H.main $ do
    -- Hero
    H.section ! A.class_ "hero" $ do
      H.h1 $ H.preEscapedToHtml ("Your startup needs technical leadership &mdash; not just another developer." :: Text)
      H.p ! A.class_ "subtitle" $ H.preEscapedToHtml ("I&rsquo;m a fractional CTO who makes the technology decisions so you can focus on growing your business. No jargon, no guesswork &mdash; just a clear technical direction." :: Text)
      H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-button" $ "Book a conversation"

    -- Who this is for
    H.section ! A.class_ "for-who" ! A.id "services" $ do
      H.h2 "Who I work with"
      H.ul ! A.class_ "card-grid" $ do
        H.li ! A.class_ "card" $ do
          H.h3 "Non-technical founders building a tech product"
          H.p $ H.preEscapedToHtml ("You have the vision and the customers, but you need someone who can translate that into the right technology choices &mdash; and make sure they get built properly." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Startups scaling past their MVP"
          H.p $ H.preEscapedToHtml ("Your agency-built prototype is breaking under real usage. You need someone to assess what you have, decide what to keep, and chart a path to a system that scales." :: Text)
        H.li ! A.class_ "card" $ do
          H.h3 "Funded startups hiring their first engineers"
          H.p $ H.preEscapedToHtml ("You&rsquo;re ready to build a team but don&rsquo;t know how to evaluate developers or set technical direction. I help you hire right and get your engineering culture started." :: Text)

    -- Entry offer
    H.section ! A.class_ "audit" $ do
      H.h2 "Start with a Technical Review"
      H.p $ H.preEscapedToHtml ("A one-day assessment of where your technology stands and what to do next. You get a written report covering:" :: Text)
      H.ul $ do
        H.li $ H.preEscapedToHtml ("How your current setup is working (and where it&rsquo;s not)" :: Text)
        H.li "The biggest risks that could slow you down or cost you money"
        H.li "What to prioritize in the next 3 months"
        H.li "Clear recommendations you can act on immediately"
      H.p $ H.preEscapedToHtml ("&euro;1,000. Fixed scope. No ongoing commitment required." :: Text)
      H.p $ H.preEscapedToHtml ("Most reviews uncover quick wins that save far more than the cost &mdash; and give you clarity on whether you need ongoing technical leadership." :: Text)
      H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-button" $ "Book a review"

    -- Social proof
    H.section ! A.class_ "results" ! A.id "results" $ do
      H.h2 "Selected work"
      H.div ! A.class_ "testimonials" $ do
        H.blockquote $
          H.p $ do
            "Helped a "
            H.strong "reinsurance technology startup"
            H.preEscapedToHtml (" build and ship their core platform &mdash; taking the product from early architecture to handling live deals in production. " :: Text)
            H.a ! A.href "https://jappie.me/the-peculiar-event-sourced-deadlock.html" $ H.preEscapedToHtml ("Read about solving a production issue &rarr;" :: Text)
        H.blockquote $
          H.p $ do
            "Served as technical lead for a "
            H.strong "construction IoT startup"
            H.preEscapedToHtml (", making architecture decisions that let them scale from pilot to production. Improved device performance 7x along the way. " :: Text)
            H.a ! A.href "https://jappie.me/stacked-against-us.html" $ H.preEscapedToHtml ("Read the full story &rarr;" :: Text)
            H.preEscapedToHtml (" &middot; " :: Text)
            H.a ! A.href "https://jappie.me/firmware-lemons.html" $ H.preEscapedToHtml ("The 7x improvement &rarr;" :: Text)
        H.blockquote $
          H.p "Mentored early-stage founders on technical strategy through accelerator programs, helping non-technical teams make confident technology decisions without overspending."

    -- Engagement model
    H.section ! A.class_ "engagement" ! A.id "approach" $ do
      H.h2 "How we work together"
      H.div ! A.class_ "card-grid" $ do
        H.div ! A.class_ "card" $ do
          H.h3 "Advisory retainer"
          H.p $ H.preEscapedToHtml ("A few hours per week of strategic guidance. I review your technical decisions, help you evaluate hires and vendors, and make sure you&rsquo;re spending your budget wisely." :: Text)
        H.div ! A.class_ "card" $ do
          H.h3 "Operational retainer"
          H.p $ H.preEscapedToHtml ("Embedded in your team 1&ndash;2 days per week. I set the technical direction, work alongside your developers, and make sure things get built right." :: Text)
        H.div ! A.class_ "card" $ do
          H.h3 "Project engagement"
          H.p "Fixed-scope builds. I design and deliver the system end to end. You get a working product, not a specification document."
      H.p ! A.class_ "engagement-note" $ H.preEscapedToHtml ("All engagements start with a conversation about your situation. I\x2019ll tell you honestly whether I can help." :: Text)

    -- About
    H.section ! A.class_ "about" $ do
      H.h2 "About"
      H.p $ H.preEscapedToHtml ("I'm Jappie Klooster. I&rsquo;ve spent years helping startups make the right technical decisions &mdash; and then building the systems to back them up. Not advice that sits in a document &mdash; decisions and delivery." :: Text)
      H.p $ H.preEscapedToHtml ("I use technologies chosen for reliability and long-term maintainability (including Haskell and Nix), but the technology is an implementation detail &mdash; what matters is that your product works, scales, and doesn&rsquo;t fall over when your users show up." :: Text)
      H.p $ do
        "For more writing and case studies, visit my "
        H.a ! A.href "/blog/" $ "blog"
        "."

    -- Final CTA
    H.section ! A.class_ "final-cta" $ do
      H.h2 $ H.preEscapedToHtml ("Let\x2019s talk about your startup" :: Text)
      H.p $ do
        H.preEscapedToHtml ("If you&rsquo;re building something and need technical leadership you can trust, " :: Text)
        H.a ! A.href "mailto:hi@jappie.me" $ "get in touch"
        H.preEscapedToHtml (". I&rsquo;m always happy to have an initial conversation &mdash; no commitment required." :: Text)
      H.a ! A.href "mailto:hi@jappie.me" ! A.class_ "cta-button" $ "Book a conversation"

-- =============================================================================
-- Blog index page (paginated listing)
-- =============================================================================

penguinBlogIndexPage :: SiteConfig -> [Article] -> PaginationInfo -> Html
penguinBlogIndexPage _config articles pagination =
  penguinBlogBaseTemplate "Blog \8212 Jappie Software B.V." $
    H.main ! A.class_ "blog-listing" $ do
      H.h1 "Blog"
      mapM_ renderBlogSummary articles
      renderPagination pagination

renderBlogSummary :: Article -> Html
renderBlogSummary article =
  H.article ! A.class_ "post-summary" $ do
    H.h2 $
      H.a ! A.href (toValue ("/blog/" <> articleUrl article)) $
        toHtml (articleTitle article)
    H.p ! A.class_ "post-meta" $ do
      H.time ! customAttribute "datetime" (formatIsoDate (articleDate article)) $
        toHtml (formatHumanDate (articleDate article))
      case articleTags article of
        [] -> mempty
        tagList -> do
          H.preEscapedToHtml (" &middot; " :: Text)
          mapM_ (\tag -> H.span ! A.class_ "tag" $ toHtml tag) tagList
    case articleSummary article of
      Just summary -> H.div ! A.class_ "summary" $ summary
      Nothing -> mempty

renderPagination :: PaginationInfo -> Html
renderPagination pagination =
  H.nav ! A.class_ "pagination" $ do
    case paginationPrevUrl pagination of
      Just url -> H.a ! A.class_ "prev" ! A.href (toValue url) $ H.preEscapedToHtml ("&larr; Newer" :: Text)
      Nothing -> mempty
    case paginationNextUrl pagination of
      Just url -> H.a ! A.class_ "next" ! A.href (toValue url) $ H.preEscapedToHtml ("Older &rarr;" :: Text)
      Nothing -> mempty

-- =============================================================================
-- Individual article page
-- =============================================================================

penguinArticlePage :: SiteConfig -> Article -> Html
penguinArticlePage _config article =
  penguinBlogBaseTemplate (articleTitle article <> " \8212 Jappie Software B.V.") $
    H.main ! A.class_ "blog-article" $
      H.article $ do
        H.header $ do
          H.h1 $ toHtml (articleTitle article)
          H.p ! A.class_ "post-meta" $ do
            H.time ! customAttribute "datetime" (formatIsoDate (articleDate article)) $
              toHtml (formatHumanDate (articleDate article))
            case articleTags article of
              [] -> mempty
              tagList -> do
                H.preEscapedToHtml (" &middot; " :: Text)
                mapM_ (\tag -> H.span ! A.class_ "tag" $ toHtml tag) tagList
        H.div ! A.class_ "entry-content" $
          articleContent article
        case articleFootnotesHtml article of
          Just fn -> fn
          Nothing -> mempty
        H.footer ! A.class_ "article-footer" $
          H.a ! A.href "/blog/" $ H.preEscapedToHtml ("&larr; Back to blog" :: Text)

-- =============================================================================
-- Helpers
-- =============================================================================

formatIsoDate :: UTCTime -> Text
formatIsoDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

formatHumanDate :: UTCTime -> Text
formatHumanDate = T.pack . formatTime defaultTimeLocale "%B %e, %Y"

voronoiScript :: Text
voronoiScript = T.unlines
  [ "const svg = d3.select(\"svg.voronoi\");"
  , "function drawVoronoi() {"
  , "  const width = window.innerWidth;"
  , "  const height = window.innerHeight;"
  , "  const area = width * height;"
  , "  const cellSize = 15000;"
  , "  const numPoints = Math.max(100, Math.ceil(area / cellSize));"
  , "  const points = d3.range(numPoints).map(() => [Math.random() * width, Math.random() * height]);"
  , "  const delaunay = d3.Delaunay.from(points);"
  , "  const voronoi = delaunay.voronoi([0, 0, width, height]);"
  , "  svg.selectAll(\"g\").remove();"
  , "  svg.append(\"g\")"
  , "    .selectAll(\"path\")"
  , "    .data(voronoi.cellPolygons())"
  , "    .join(\"path\")"
  , "      .attr(\"d\", d => \"M\" + d.join(\"L\") + \"Z\")"
  , "      .attr(\"stroke\", \"#ddd\")"
  , "      .attr(\"fill\", \"none\");"
  , "}"
  , "drawVoronoi();"
  , "let resizeTimer;"
  , "window.addEventListener(\"resize\", function() {"
  , "  clearTimeout(resizeTimer);"
  , "  resizeTimer = setTimeout(drawVoronoi, 150);"
  , "});"
  ]
