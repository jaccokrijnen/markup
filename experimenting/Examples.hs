module Example where

import ClassContinuations

-- example of better notation
s :: Parser Brackets
s =       rhs   semPair  '(' s ')' s
      <|> rhs   semEmpty ""

      where semPair  = Pair
            semEmpty = const Empty 
