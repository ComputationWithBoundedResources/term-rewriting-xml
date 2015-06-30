-- |

module Data.Rewriting.Term.Xml
( xmlToTerm
) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc

import qualified Data.Rewriting.Term.Type as T
import qualified Data.Rewriting.Term.Ops as O

import Data.Maybe

xmlToTerm :: UNode String -> (T.Term String String, [String])
xmlToTerm xmlTerm = let
  term = xmlToTerm' xmlTerm
  vars = O.vars term
  in (term,vars)

xmlToTerm' :: UNode String -> T.Term String String
xmlToTerm' xmlTerm
  | ename == "funapp" = xmlToFunction xmlTerm
  | ename == "var"    = xmlToVariable xmlTerm
  | otherwise        = error $ "Data.Rewriting.Term.Xml.xmlToTerm: eName not matching \"" ++
                       ename ++ "\"" 
  where
    ename = eName xmlTerm



xmlToFunction :: UNode String -> T.Term String String
xmlToFunction xmlFunapp = T.Fun name args
  where
    name = getName $ head $ eChildren $ fromJust $ findChild "name" xmlFunapp
    xmlArgs = map getArgTerm $ findChildren "arg" xmlFunapp
    args = map xmlToTerm' xmlArgs
    getName (Text t) = t
    getArgTerm = head.eChildren

xmlToVariable :: UNode String -> T.Term String String
xmlToVariable xmlVar = T.Var name
  where
    name = getName $ head $ eChildren xmlVar
    getName (Text t) = t 
