-- | Xml parser for problem type
module Data.Rewriting.Problem.Xml
( xmlFileToProblem
, xmlBSToProblem
--, xmlToProblem
)  where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc

import qualified Data.ByteString.Lazy as L
import Data.Maybe

import qualified Data.Rewriting.Problem.Type as T

import Data.Rewriting.Rule.Xml

xmlFileToProblem :: FilePath -> IO (T.Problem String String)
xmlFileToProblem filename =
  xmlBSToProblem `fmap` L.readFile filename

xmlBSToProblem :: L.ByteString -> T.Problem String String
xmlBSToProblem xmlbs = let
  (xmlData, mErr) = parse defaultParseOptions xmlbs -- :: (UNode String, Maybe XMLParseError)
  problem = xmlToProblem xmlData
  in case mErr of
      Nothing -> problem
      Just err -> error $ "XML parse failed: "++show err

xmlToStrategy :: UNode String -> T.Strategy
xmlToStrategy xmlStrategy =
  case concatMap unText $ eChildren xmlStrategy of
    "INNERMOST" -> T.Innermost
    "OUTERMOST" -> T.Outermost
    "FULL"      -> T.Full
    s           -> error $ "Data.Rewriting.Problem.Xml.xmlToStrategy: unexpcted strategy: " ++ s
  where
    unText (Text t) = t
    unText _        = error $ "Data.Rewriting.Problem.Xml.xmlToStrategy: something is wrong."

xmlToStartTerms :: Maybe (UNode String) -> T.StartTerms
xmlToStartTerms xmlStartTerms =
  maybe T.AllTerms getStartTerm xmlStartTerms
  where
    getStartTerm xst = case eName $ head $ eChildren xst of
      "constructor-based" -> T.BasicTerms
      "full"              -> T.AllTerms
      s                   -> error $ "Data.rewriting.Problem.ParseXml.xmlToStartTerms: unexpected start terms:" ++ s


xmlToProblem :: UNode String -> T.Problem String String
xmlToProblem xmlProblem = let
  xmlTrs = fromJust $ findChild "trs" xmlProblem
  xmlStrategy = fromJust $ findChild "strategy" xmlProblem
  mxmlStartTerms = findChild "startterm" xmlProblem
  (rulesPair, variables, fullSignature) = xmlToRulesPair xmlTrs
  strategy = xmlToStrategy xmlStrategy
  startTerms = xmlToStartTerms mxmlStartTerms
  symbols = map fst fullSignature
  rules = T.RulesPair {T.strictRules = fst rulesPair, T.weakRules = snd rulesPair}
  in T.Problem { T.startTerms = startTerms
               , T.strategy = strategy
               , T.rules = rules
               , T.variables = variables
               , T.symbols = symbols
               , T.signature = Nothing
               , T.theory = Nothing
               , T.comment = Nothing}

