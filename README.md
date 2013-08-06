# markupconverter (W.I.P.)

_Mix and match your favorite grammar fragments and aspects to customize your own markup translator!_

markupconverter is an extensible Haskell library for converting markup language formats.


## How?

To translate from language A to B, we need two things:

1. __Grammar fragments__ to describe the parser for language A, that parses a document into a general document datatype.

2. __Aspects__ to describe the process of converting the general document datatype to a document in language B. 

To build the translator, we write

    translAB = buildTranslator grammarA aspectsB

## Extensible?
Both grammar fragments and aspects are first class Haskell values that can be imported, manipulated and exported again. For example:

    translAB' = buildTranslator (grammarA +>> myGramExtension) (aspectsB .+. myNewAspect .+. anotherAspect) 

## Grammar fragments
By using the [murder](http://google.nl) library, we can describe parsers by writing __context free grammars__

## Aspects