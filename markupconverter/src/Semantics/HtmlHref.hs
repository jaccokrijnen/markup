{-# LANGUAGE TemplateHaskell, EmptyDataDecls, NoMonomorphismRestriction #-}
module Semantics.HtmlHref where

import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1

import Language.Grammars.AspectAG

import Decl.DocumentHref
import Semantics.Html


href_output  = syn output $ 
                    do adr   <- at ch_href_address
                       descr <- at ch_href_description
                       return $ "<a href = " ++ adr ++ ">" 
                              ++ descr 
                              ++ "</a>"


aspHref = href_output

semHtmlHref = mkDocHref aspHref