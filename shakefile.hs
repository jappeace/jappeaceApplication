{-# LANGUAGE ScopedTypeVariables #-}

import Development.Shake
import Development.Shake.FilePath
import System.Exit
import Control.Monad
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.Class
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Pandoc.Lens
import Control.Lens
import Data.Text.Lens
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html
import Data.Text(Text)
import Data.Foldable

defaultReaderOptions :: ReaderOptions
defaultReaderOptions = def

defaultWriterOptions :: WriterOptions
defaultWriterOptions = def

statusTraverse :: Traversal' Pandoc Inline
statusTraverse = droppingWhile (\x ->
                    Just (Text.pack "status:") /= preview (_Str . packed . to Text.toLower) x
                  ) (body . ix 0 . _Para . traversed)

data Status = Draft Html
            | Live Html

renderPost :: Status -> Text
renderPost (Draft html) =  renderHtml $ do
    textComment (Text.pack "draft")
    html
renderPost (Live live) = renderHtml $ do
    textComment (Text.pack "live")
    html

main :: IO ()
main = shakeArgs shakeOptions $ do
    phony "build" build
    "build/content/md" %> \out -> do
        let src = "content"

        files :: [String] <- map ("content" </>) <$> getDirectoryFiles "content" ["*.md"]

        liftIO $ print files

        need files

        res <- forM files $ \file -> liftIO $ do
            contents <- Text.readFile file
            runIOorExplode $ do
              readPandoc <- readMarkdown defaultReaderOptions contents
              let tag = if has (statusTraverse . _Str . only "draft") readPandoc then Draft else Live
              tag <$> writeHtml5 defaultWriterOptions readPandoc

        traverse_ (liftIO . Text.putStrLn . renderPost) res




build :: Action ()
build = do
  assertBranchClean

assertBranchClean :: Action ()
assertBranchClean = do
  exitCode <- cmd "git diff-index --quiet HEAD"
  unless (exitCode == ExitSuccess) $ error "branch dirty, commit first"
