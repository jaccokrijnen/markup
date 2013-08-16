{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Decl.DocumentHref where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive



data EXT_Inline = Href { href_address :: String, href_description :: String }

$(extendAG ''EXT_Inline [ ])
$(deriveLang "DocHref" [''EXT_Inline])