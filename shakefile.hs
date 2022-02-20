import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
    phony "build" build

build :: Action ()
build = do
  assertBranchClean

assertBranchClean :: Action ()
assertBranchClean =
  cmd_ "git diff-index --quiet HEAD -- || (echo \"branch dirty, commit first\" && false)"
