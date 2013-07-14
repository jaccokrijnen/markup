{-# OPTIONS -fcontext-stack=100 #-}

{-# LANGUAGE  Arrows, DoRec, TemplateHaskell #-}

module Language.Oberon0.L1.Gram where


import Language.Oberon0.L1.Decl
 
import Control.Arrow
import Control.Applicative
import Data.Set (fromList)

import Language.Grammars.Murder 
import Language.Grammars.Murder.Derive
import Language.Grammars.Grammar
import Language.Grammars.AspectAG

$(csLabels  [ "cs_Module", "cs_Declarations", "cs_Expression", "cs_Factor", "cs_StatementSequence"
            , "cs_Statement", "cs_Ident", "cs_IdentL", "cs_MaybeElseStmt", "cs_Type"])


eExp  sf = pIntCmpExp sf (sem_Lit ECmp)
neExp sf = pIntCmpExp sf (sem_Lit NECmp)
lExp  sf = pIntCmpExp sf (sem_Lit LCmp)
leExp sf = pIntCmpExp sf (sem_Lit LECmp)
gExp  sf = pIntCmpExp sf (sem_Lit GCmp)
geExp sf = pIntCmpExp sf (sem_Lit GECmp)

plusExp  sf = pIntBOpExp sf (sem_Lit Plus)
minusExp sf = pIntBOpExp sf (sem_Lit Minus)
timesExp sf = pIntBOpExp sf (sem_Lit Times)
divExp   sf = pIntBOpExp sf (sem_Lit Div)
modExp   sf = pIntBOpExp sf (sem_Lit Mod)


posExp sf = pIntUOpExp sf (sem_Lit Ps)
negExp sf = pIntUOpExp sf (sem_Lit Ng)


orExp   sf = pBoolBOpExp sf (sem_Lit Or)
andExp  sf = pBoolBOpExp sf (sem_Lit And)

notExp  sf = pBoolUOpExp sf (sem_Lit Not)

trueExp  sf t = pBoolExp sf  (\r -> DTerm (pos (t r)) True)
falseExp sf f = pBoolExp sf  (\r -> DTerm (pos (f r)) False)

l1 sf = proc () -> do

         rec

                 modul      <- addNT -<  iI (pModule sf)  "MODULE" ident ";" 
                                                          decls 
                                                          (pMaybe (pEmptyStmt sf, id) (iI "BEGIN" ss Ii)) 
                                                          "END" ident "." Ii 

                 decls      <- addNT -<  iI (pDeclarations sf) 
                                            (pMaybe (pDeclL_Nil sf, id) (iI "CONST" cstDeclL Ii))
                                            (pMaybe (pDeclL_Nil sf, id) (iI "TYPE"  typDeclL Ii))
                                            (pMaybe (pDeclL_Nil sf, id) (iI "VAR"   varDeclL Ii))
                                         Ii

                 cstDeclL   <- addNT -<  pFoldr (pDeclL_Cons sf, pDeclL_Nil sf) (iI (pCstDecl sf) ident "=" exp ";" Ii)

                 typDeclL   <- addNT -<  pFoldr (pDeclL_Cons sf, pDeclL_Nil sf) (iI (pTypDecl sf) ident "=" typ ";" Ii)

                 varDeclL   <- addNT -<  pFoldr (pDeclL_Cons sf, pDeclL_Nil sf) (iI (pVarDecl sf) idL ":" typ ";" Ii)

                 idL        <- addNT -<  iI (pIdentL_Cons sf) ident (pFoldr (pIdentL_Cons sf, pIdentL_Nil sf) (iI "," ident Ii)) Ii

                 typ        <- addNT -<  iI (pType sf) ident Ii


                 exp        <- addNT -<  iI sexp Ii <|>
                                         iI (eExp sf) exp "=" sexp Ii <|> iI (neExp sf) exp "#" sexp Ii <|>
                                         iI (lExp sf) exp "<" sexp Ii <|> iI (leExp sf) exp "<" "=" sexp Ii <|>
                                         iI (gExp sf) exp ">" sexp Ii <|> iI (geExp sf) exp ">" "=" sexp Ii


                 sexp       <- addNT -<  iI signed Ii <|> 
                                         iI (plusExp sf) sexp "+" signed Ii <|> iI (minusExp sf) sexp "-" signed Ii <|>  
                                         iI (orExp sf) sexp "OR" signed Ii

                 signed     <- addNT -<  iI (posExp sf) "+" term Ii <|> iI (negExp sf) "-" term Ii <|> iI term Ii


                 term       <- addNT -<  iI factor Ii <|>
                                         iI (timesExp sf) term "*"   factor Ii <|> iI (divExp sf)  term "DIV" factor Ii <|> 
                                         iI (modExp sf)   term "MOD" factor Ii <|> iI (andExp sf)  term "&"   factor Ii



                 factor     <- addNT -<  iI (trueExp sf) (kw "TRUE") Ii  <|>  iI (falseExp sf) (kw "FALSE") Ii <|> 
                                         iI (pIdExp sf)  ident Ii   <|>  iI (pIntExp sf) int Ii  <|>
                                         iI (pParExp sf) "(" exp ")" Ii  <|> iI (notExp sf) "~" factor Ii


                 ss         <- addNT -<  iI (pSeqStmt sf) statement (pFoldr (pSeqStmt sf, pEmptyStmt sf) (iI ";" statement Ii)) Ii

                 statement  <- addNT -<  iI (pAssigStmt sf) ident  ":" "=" exp Ii   <|> 
                                         iI (pIfStmt sf) "IF" cond  
                                                         (pFoldr (pCondStmtL_Cons sf, pCondStmtL_Nil sf) (iI "ELSIF" cond Ii))  
                                                         mbelse 
                                                         "END" Ii   <|> 
                                         iI (pWhileStmt sf) "WHILE" exp "DO" ss "END" Ii  <|>
                                         iI (pEmptyStmt sf) Ii
                 cond       <- addNT -<  iI (pCondStmt sf) exp "THEN" ss Ii

                 ident      <- addNT -<  iI var Ii <|> iI con Ii

                 mbelse     <- addNT -<  pMaybe (pMaybeElseStmt_Nothing sf, pMaybeElseStmt_Just sf) (iI "ELSE" ss Ii)

         exportNTs -< exportList modul  $   export cs_Module             modul
                                        .   export cs_Declarations       decls
                                        .   export cs_Expression         exp
                                        .   export cs_Factor             factor
                                        .   export cs_StatementSequence  ss
                                        .   export cs_Statement          statement
                                        .   export cs_Ident              ident
                                        .   export cs_IdentL             idL
                                        .   export cs_MaybeElseStmt      mbelse
                                        .   export cs_Type               typ

l1Kws = fromList ["MODULE", "BEGIN", "END", "CONST", "TYPE", "VAR", "OR", "DIV", "MOD", "IF", "ELSIF", "ELSE", "WHILE", "DO", "THEN"]

