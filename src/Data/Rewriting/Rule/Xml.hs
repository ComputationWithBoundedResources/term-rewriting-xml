-- | 

module Data.Rewriting.Rule.Xml
( xmlToRulesPair
) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc

import Data.Maybe (fromJust)
import Data.List (union)

import qualified Data.Rewriting.Rule.Type as R



import Data.Rewriting.Term.Xml

xmlToRule :: UNode String -> (R.Rule String String, [String])
xmlToRule xmlRule = (R.Rule lhs rhs, vars)
  where
    (lhs,lvars) = xmlToTerm $ getXmlTerm $ fromJust $ findChild "lhs" xmlRule
    (rhs,rvars) = xmlToTerm $ getXmlTerm $ fromJust $ findChild "rhs" xmlRule
    getXmlTerm = head . eChildren
    vars = union lvars rvars

xmlToRules :: [UNode String] -> ([R.Rule String String], [String])
xmlToRules xmlRuleList = let
  (rules,varss) = unzip $ map xmlToRule xmlRuleList
  in (rules, foldr union [] varss)
  
xmlToRulesPair :: UNode String -> (([R.Rule String String],[R.Rule String String]), [String], [(String,Int)])
xmlToRulesPair xmlTrs = ((strictRules, weakRules), vars, signature)
  where
    xmlRules = findChildren "rules" xmlTrs
    xmlStrictRuleList = concat $ map (findChildren "rule") xmlRules
    xmlWeakRuleList = concat $ map (findChildren "rule") $
                      concat $ map (findChildren "relrules") xmlRules
    xmlSignature = fromJust $ findChild "signature" xmlTrs
    (strictRules,varss) = xmlToRules xmlStrictRuleList
    (weakRules,varsw) = xmlToRules xmlWeakRuleList
    signature = xmlToSignature xmlSignature
    vars = union varss varsw

xmlToSignature :: UNode String -> [(String,Int)]
xmlToSignature xmlSig = map xmlToSymbol xmlSyms
  where
    xmlSyms = findChildren "funcsym" xmlSig

xmlToSymbol :: UNode String -> (String,Int)
xmlToSymbol xmlSym = (name,arity)
  where
    getText (Text t) = t
    name = concatMap getText $ eChildren $ fromJust $ findChild "name" xmlSym
    arity = read $ concatMap getText $ eChildren $ fromJust $ findChild "arity" xmlSym
