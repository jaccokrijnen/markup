module Preprocessors (preprocessHtml) where

import Data.Char


-- html
preprocessHtml :: String -> String
preprocessHtml ""                 = ""
preprocessHtml ('>':xs) 
             | plainPrefix xs = let (text, xs') = getPlain xs
                                in  "><plain>" ++ text ++ "</plain>" ++ preprocessHtml xs'
preprocessHtml (x  :xs) = x:(preprocessHtml xs) 


plainPrefix :: String -> Bool
plainPrefix xs = case dropWhile isSpace xs of
                     (x:_) -> x /= '<'
                     _     -> False

getPlain :: String -> (String, String)
getPlain xs = (takeWhile (/= '<') xs, dropWhile (/= '<') xs)  
