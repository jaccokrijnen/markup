module Latex where

headers = [(2,"section"), (3, "subsection"), (4, "subsubsection")]

command name pBody = iI ("\\" ++ name ++ "{") inlineL "}" Ii

gLatex sem = proc () -> do
    rec
        root         <-addNT-< iI (pDocument sem) blockL Ii

        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) (command "paragraph" InlineL) Ii
        header       <-addNT-< foldr1 (<|>) . flip map headers $ \(x, name) -> 
                                   iI (pHeader sem x) (command name inlineL) Ii

        inline       <-addNT-< iI (pPlain   sem) (command "plain"  (someExcept "}")) Ii
                        <|>    iI (pBold    sem) (command "textbf"  inlineL        ) Ii
                        <|>    iI (pItalics sem) (command "textit"  inlineL        ) Ii
        inlineL      <-addNT-< pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                   iI inline Ii