module Document where


data Document = Document [Block]

data Block = Header    Int Line
		   | Paragraph [Line]
		   deriving (Show)

data Line = Plain String
		  | Bold Line
		  | Italics Line
		  deriving (Show)		  