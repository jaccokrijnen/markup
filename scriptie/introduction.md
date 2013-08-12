# Building an extensible markup convertor
### a tutorial in compositional compiler construction

## Abstract
...

## Introduction
Markup languages are diverse and have different purposes, such as blogs, publications and documentation. Yet different situations require different languages: we use latex to write a paper, markdown to document a repository and html to write a webpage. 

A markup converter is a tool that converts a document in language X to a document in language Y, where X and Y are markup languages. To do so, we require two things: 

* a way of describing the _parser_ for language X. For this we use _grammar fragments_ from the `murder` library.
* a way of describing the _semantics_ of language Y. For this we we use _aspects_ from the `aspectag` library.

We want these two to be completely decoupled, so that we can write:

	latex2html :: String -> (String, [Message])
    latex2html = buildConverter gramLatex aspHtml

Grammar fragments will be explained in section [], aspects will be covered in section []. 

Also, we would like this tool to be highly extensible, so developers can define their own plugins and modifications, like so:

    import Grammars.Latex
    import Semantics.Html
    
    -- support for hyperlinks like <a href="http://google.com">click here</a>
	gramLatexLinks = ...
	aspHtmlLinks = ...

    latex2html' = buildConverter (gramLatex +>> gramLatexLinks) (aspHtml .+. aspHtmlLinks)

Here we see the initial grammar and initial aspect being extended with `+>>` and `.+.` respectively. 












## 2. Grammars and the murder library

We assume that the reader is familiar with applicative-style parsing and the basics of context-free grammars.

The fundamental difference between conventional parsing libraries is that the `murder` library let's us write values of type `Grammar a` instead of the common `Parser a`. A `Grammar a` represents a collection of productions and a start non terminal, similar to a context-free grammar. But what makes this Grammar type so useful? 

First of, a `Grammar a` is _extensible_: it is possible to retrieve the individual productions, make modifications, define new productions and build an extended `Grammar a`. As a consequence, murder can also perform analysis and optimizations such as left recursion removal and empty productions removal. 

Secondly, murder makes optimal use of the Haskell type system to keep the seperate productions typed and give static garantuees that composing grammars happens in a valid way. 

Finally, we can map such a `Grammar a` onto a `Parser a` from the `uu-parsinglib` by using the function ``compile :: Grammar a -> Parser a`` as defined by murder, which performs the aforementioned optimizations.

### 2.1 Productions


is a heterogenous collection of ``Productions l a env``, as in  so called grammar fragments. These fragments can be composed with the ``+>>`` operator, as demonstrated in section [intro]. 

For simplicity, we  


### 2.2 Grammar fragments