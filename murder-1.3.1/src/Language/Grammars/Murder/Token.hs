{-# LANGUAGE TypeSynonymInstances #-}
module Language.Grammars.Murder.Token ( Token,EnumValToken(..)
             , module UU.Scanner.GenToken
             , reserved,valueToken,errToken,tokTpIsInt,tokTpIsId, tokTpQual) where 

import UU.Scanner.Position
import UU.Scanner.GenToken

import Data.Set 


type Token = GenToken String EnumValToken String


data EnumValToken
  = TkVarid
  | TkConid
  | TkOp
  | TkConOp
  | TkQVarid
  | TkQConid
  | TkQOp
  | TkQConOp
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkFraction
  | TkTextnm
  | TkTextln 
  | TkError
  deriving (Eq, Ord)

instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key ++ maybeshow pos
         ValToken tp val   pos -> show tp ++ " " ++ val ++ maybeshow pos
       )

instance Show EnumValToken where
 show tp = case tp of       
  TkVarid      -> "lower case identifier" 
  TkConid      -> "upper case identifier" 
  TkOp         -> "operator"  
  TkConOp      -> "con operator"  
  TkQOp        -> "qualified operator"  
  TkQVarid     -> "lower case qualified identifier" 
  TkQConid     -> "upper case qualified identifier" 
  TkQConOp     -> "qualified con operator"            
  TkString     -> "string"              
  TkChar       -> "character"            
  TkInteger8   -> "octal integer"         
  TkInteger10  -> "decimal Integer"       
  TkInteger16  -> "hexadecimal integer"   
  TkFraction   -> "fraction (float,...)"   
  TkTextnm     -> "text name"             
  TkTextln     -> "text lines"             
  TkError      -> "error in scanner:"   

maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn

reserved                :: String -> Pos -> Token
reserved                =  Reserved 

valueToken              :: EnumValToken -> String -> Pos -> Token
valueToken              =  ValToken 

errToken                :: String -> Pos -> Token
errToken                =  valueToken TkError 

tokTpIsInt :: EnumValToken -> Bool
tokTpIsInt tp = tp == TkInteger8 || tp == TkInteger10 || tp == TkInteger16

tokTpIsId :: EnumValToken -> Bool
tokTpIsId
  = (`member` ts)
  where ts = fromList
  			   [TkVarid,TkConid,TkOp,TkConOp
  			   ,TkQVarid,TkQConid,TkQOp,TkQConOp
  			   ]

tokTpQual :: EnumValToken -> EnumValToken
tokTpQual TkVarid = TkQVarid
tokTpQual TkConid = TkQConid
tokTpQual TkOp    = TkQOp
tokTpQual TkConOp = TkQConOp
tokTpQual t       = t

