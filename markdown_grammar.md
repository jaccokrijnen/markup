# Markdown grammaticaen notes

1. grammatica afgeleid van [daringfireball](http://daringfireball.net/projects/markdown/syntax)
2. nog simpel/ambigu

probleem: markdown is niet echt een taal: elke reeks van characters is een "geldig" markdown document (=> gebruik error correctie (insertions) van uuparsinglib). 

## Document level
    DOC ::= BLOCK*

## Block level

    BLOCK ::= PARS | HEADER | BLQUOTE | ...

 
### Paragraphs
lines met tekst, "blank lines" om te scheiden
    
    PARS    ::= PAR (BLINE* PAR)*
    PAR     ::= TLINE+
    BLINE   ::= WSPACE* NEWLINE
    TLINE   ::= (not NEWLINE)* NEWLINE


### Headers
setext, en atx

    HEADER  ::= SETEXT | ATX
    SETEXT  ::= TLINE ('='+ | '-'+) NEWLINE\
    ATX     ::= '#'+ TLINE


### Blockquotes 
Lines kunnen beginnen met `>`, met erachter geldige markdown. Niet fijn parsen, op te lossen door `QINDENT` en `QDEDENT` tokens te genereren bij scanner.  

    BLQUOTE ::= ( MARKDOWN)+ | '>' PAR

> quote
> 
>> niveau dieper
>
> terug  



### inline
bold, italics, underline, evt html entities (&copy; &lt;)

    ENT ::= & (a..z)* ;`  

`&` wordtvertaald naar `&emp;`, `<` toegestaan, behalve als onderdeel van inline html tag (maar binnen code geen inline html mogelijk, dus `<` altijd als `<`.



* **embedded html** 
	* block level (div, table ...): bevat geen markdown, pure html die geinlined kan worden
	
	* span level (span, cite, del): mag wel markdown bevatten binnen tags 