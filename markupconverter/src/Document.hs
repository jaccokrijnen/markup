module Document where

data Root = Root { document :: Document }

data Document = Document { blocks :: [Block] }

data Block = Header    { hlevel :: Int,
                         hInlines :: [Inline] }
           | Paragraph { pInlines :: [Inline] }
           deriving (Show)

data Inline = Plain   { str_plainInl     :: String }
            | Bold    { inlines_boldInl :: [Inline] }
            | Italics { inlines_italInl  :: [Inline]}
            deriving (Show)       