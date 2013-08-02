module Document where

data Root = Root { document :: Document }
    deriving Show

data Document = Document { blocks :: [Block] }
    deriving Show

data Block = Header    { level_head :: Int,
                         inlines_head :: [Inline] }
           | Paragraph { inlines_par :: [Inline] }
           deriving (Show)

data Inline = Plain   { str_plainInl     :: String }
            | Bold    { inlines_boldInl :: [Inline] }
            | Italics { inlines_italInl  :: [Inline]}
            deriving (Show)       