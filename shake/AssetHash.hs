{-# LANGUAGE OverloadedStrings #-}

-- | Content-hash cache-busting voor de statische assets van
-- webwinkelverhuis.nl: de pure naamgeving en de HTML-herschrijving. De
-- IO-kant (bestanden lezen en de gehashte kopie schrijven) staat in de
-- Shakefile; dit module is bewust puur zodat de testsuite het kan
-- aanspreken.
module AssetHash
  ( GehashteAssets(..)
  , gehashteAssetNaam
  , herschrijfAssetVerwijzingen
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

-- | De cache-bestendige namen van de stylesheets en Elm-bundels van
-- webwinkelverhuis.nl: de inhoudshash zit in de bestandsnaam, zodat een
-- browser na een deploy nooit een oude versie uit zijn cache haalt.
data GehashteAssets = GehashteAssets
  { gehashteStyleCss :: Text
  , gehashteBlogCss :: Text
  , gehashtePrijsCalculatorJs :: Text
  , gehashteScannerFormJs :: Text
  }

-- | De cache-bestendige bestandsnaam voor een asset: de basisnaam plus de
-- eerste 10 tekens van de sha256 van de inhoud, plus de extensie
-- ("style-ab12cd34ef.css").
gehashteAssetNaam :: String -> String -> ByteString -> String
gehashteAssetNaam basisnaam extensie inhoud =
  basisnaam
    <> "-"
    <> take 10 (showDigest (sha256 (LBS.fromStrict inhoud)))
    <> "."
    <> extensie

-- | Herschrijf de logische assetnamen in een gerenderde pagina naar hun
-- gehashte tegenhangers uit 'GehashteAssets'.
herschrijfAssetVerwijzingen :: GehashteAssets -> TL.Text -> TL.Text
herschrijfAssetVerwijzingen gehashte =
    vervangAsset "/style.css" (gehashteStyleCss gehashte)
  . vervangAsset "/blog.css" (gehashteBlogCss gehashte)
  . vervangAsset "/prijs-calculator.js" (gehashtePrijsCalculatorJs gehashte)
  . vervangAsset "/scanner-form.js" (gehashteScannerFormJs gehashte)

vervangAsset :: Text -> Text -> TL.Text -> TL.Text
vervangAsset logischeNaam gehashteNaam =
  TL.replace (TL.fromStrict logischeNaam) (TL.fromStrict ("/" <> gehashteNaam))
