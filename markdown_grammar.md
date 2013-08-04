# Markdown grammaticaen notes

* grammatica afgeleid van [daringfireball](http://daringfireball.net/projects/markdown/syntax)
* nu nog simpel en ambigu 


probleem: markdown is niet echt een taal: elke reeks van characters is een "geldig" markdown document => error correctie (insertions) van uuparsinglib ?

## Block level
	MARKDOWN ::= BLOCK*
    BLOCK    ::= PAR | HEADER | BLQUOTE | ...
 
### Paragraphs
lines met tekst, "blank lines" om te scheiden

    PAR     ::= TLINE+ BLINE
    BLINE   ::= WSPACE* NEWLINE
    TLINE   ::= (not NEWLINE)* NEWLINE


### Headers
setext, en atx

    HEADER  ::= SETEXT | ATX
    SETEXT  ::= TLINE ('='+ | '-'+) NEWLINE
    ATX     ::= '#'+ TLINE


### Blockquotes 
Lines kunnen beginnen met `>`, met erachter geldige markdown. Niet fijn parsen, op te lossen door `QSTART` en `QSTOP` tokens te genereren bij scanner.  

    BLQUOTE ::= QSTART DOC QSTOP

> quote
> 
>> niveau dieper
>
> terug  



## Inline
bold, italics, underline, evt html entities (&copy; &lt;)

    ENT ::= & (a..z)* ;`  

`&` wordtvertaald naar `&emp;`, `<` toegestaan, behalve als onderdeel van inline html tag (maar binnen code geen inline html mogelijk, dus `<` altijd als `<`.



* **embedded html** 
	* block level (div, table ...): bevat geen markdown, pure html die geinlined kan worden
	
	* span level (span, cite, del): mag wel markdown bevatten binnen tags 