{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Document where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.Derive


data Document = Document { blocks :: BlockL }
    deriving Show


type BlockL = [Block]

data Block = Header    { level_header   :: Int,
                         inlines_header :: InlineL }
           | Paragraph { inlines_par  :: InlineL }
           deriving (Show)


type InlineL = [Inline]

data Inline = Plain   { str_plainInl     :: String }
            | Bold    { inlines_boldInl :: InlineL }
            | Italics { inlines_italInl  :: InlineL }
            deriving (Show)       





$(deriveAG ''Document)