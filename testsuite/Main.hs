-- | 

module Main
where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

import Control.Monad (filterM,mapM)

import qualified System.Directory as D
import System.Exit (exitFailure,exitSuccess)
import qualified System.FilePath as F
import qualified System.Environment as E

import Data.List as L
import qualified Data.Set as Set

import qualified Data.Rewriting.Problem.Xml as X
import qualified Data.Rewriting.Problem.Parse as W

import qualified Data.Rewriting.Problem.Type as T


getFilenames :: FilePath -> IO [FilePath]
getFilenames fpxml = do
  allDir <- D.getDirectoryContents fpxml
  let allFDir = map (F.combine fpxml) (filter (`notElem` [".",".."]) allDir )
  xmlfns <- filterM D.doesFileExist allFDir
  subdirs <- filterM D.doesDirectoryExist allFDir
  subfiless <- mapM getFilenames subdirs -- infinite recursion with symbolic links possible
  let files = xmlfns ++ concat subfiless
  return files

getFileTuples :: FilePath -> FilePath -> IO [(FilePath,FilePath)]
getFileTuples fpxml fptrs = do
  xmlfns <- getFilenames fpxml
  let trsfns = map ((flip F.replaceExtension "trs") . (fptrs ++) . drop (length fpxml)) xmlfns
  return $ zip xmlfns trsfns
  
checkStartTerms p1 p2 = let
  st1 = T.startTerms p1
  st2 = T.startTerms p2
  in st1 == st2

checkRules p1 p2 = let
  rp1 = T.rules p1
  rp2 = T.rules p2
  wr1 = T.weakRules rp1
  wr2 = T.weakRules rp2
  sr1 = T.strictRules rp1
  sr2 = T.strictRules rp2
  in (null (wr1 \\ wr2)) &&
     (null (sr1 \\ sr2)) &&
     (null (wr2 \\ wr1)) &&
     (null (sr2 \\ sr1))

checkStrategy p1 p2 = let
  s1 = T.strategy p1
  s2 = T.strategy p2
  in s1 == s2

checkVaraibles p1 p2 = let
  v1 = Set.fromList $ T.variables p1
  v2 = Set.fromList $ T.variables p2
  in v1 == v2

checkSymbols p1 p2 = let
  s1 = Set.fromList $ T.symbols p1
  s2 = Set.fromList $ T.symbols p2
  in s1 == s2

prop_sameSystem :: (FilePath,FilePath) -> Property
prop_sameSystem ft = monadicIO $ do
  --run $ appendFile "tested" $ show ft ++ "\n"
  p1 <- run $ X.xmlFileToProblem (fst ft)
  p2 <- run $ W.parseFileIO (snd ft)
  assert $ and [ checkRules p1 p2
               -- , checkStartTerms p1 p2 -- removed since Information not available in WST-format
               , checkStrategy p1 p2
               , checkVaraibles p1 p2
               , checkSymbols p1 p2]

prop_allSameSystem :: [(FilePath,FilePath)] -> Property
prop_allSameSystem fts = forAll (elements fts) prop_sameSystem


main :: IO ()
main = do
  args <- E.getArgs
  let qcArgs = stdArgs { maxSuccess = read (args !! 0)}
  fts <- getFileTuples (args !! 1) (args !! 2)
  result <- quickCheckWithResult qcArgs (prop_allSameSystem fts)
  case result of
    Success {} -> exitSuccess
    _ -> exitFailure

