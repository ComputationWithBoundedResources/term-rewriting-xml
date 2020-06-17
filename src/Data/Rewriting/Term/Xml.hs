-- |

module Data.Rewriting.Term.Xml
( xmlToTerm
) where

import           Text.XML.Expat.Proc
import           Text.XML.Expat.Tree

import qualified Data.Rewriting.Term.Ops  as O
import qualified Data.Rewriting.Term.Type as T

import           Data.Maybe

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
    name = concatMap unText $ eChildren $ fromJust $ findChild "name" xmlFunapp
    xmlArgs = map getArgTerm $ findChildren "arg" xmlFunapp
    args = map xmlToTerm' xmlArgs
    unText (Text t) = t
    unText _        = error "Data.Rewriting.Term.Xml.xmlToFunction: somehting is wrong."
    getArgTerm = head.eChildren

xmlToVariable :: UNode String -> T.Term String String
xmlToVariable xmlVar = T.Var name
  where
    name = concatMap unText $ eChildren xmlVar
    unText (Text t) = t
    unText _        = error "Data.Rewriting.Term.Xml.xmlToVariabl: somehting is wrong."
