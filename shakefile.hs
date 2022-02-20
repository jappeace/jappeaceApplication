import Development.Shake
import Development.Shake.FilePath
import System.Exit
import Control.Monad

main :: IO ()
main = shakeArgs shakeOptions $ do
    phony "build" build

build :: Action ()
build = do
  assertBranchClean

assertBranchClean :: Action ()
assertBranchClean = do
  exitCode <- cmd "git diff-index --quiet HEAD"
  unless (exitCode == ExitSuccess) $ error "branch dirty, commit first"
