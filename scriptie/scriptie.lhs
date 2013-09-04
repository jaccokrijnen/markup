

\documentclass{article}
\usepackage[hmargin=3cm,vmargin=3.5cm]{geometry}

%format proc       = "\mathbf{proc}"
%format rec        = "\mathbf{rec}"
%format rec        = "\mathbf{rec}"
%format >>>        = "\mathbin{\text{\ttfamily{>>>}}}"
%format <-        = "\mathbin{\text{\ttfamily{<-}}}"
%format ->        = "\mathbin{\text{\ttfamily{->}}}"
%format -<         = "\prec{}"

%format  <*        = "\mathbin{\text{\small\ttfamily{<*}}}"
%format  <*>       = "\mathbin{\text{\small\ttfamily{<*>}}}"
%format  <**>      = "\mathbin{\text{\small\ttfamily{<**>}}}"
%format <??>       = "\mathbin{\text{\ttfamily{<??>}}}"
%format  <|>       = "\mathbin{\text{\small\ttfamily{<|>}}}"
%format  <$>       = "\mathbin{\text{\small\ttfamily{<\$>}}}"
%format  <$        = "\mathbin{\text{\small\ttfamily{<\$}}}"
%format  iI        = "\mathbin{\text{\small\ttfamily{iI}}}"  
%format  Ii        = "\mathbin{\text{\small\ttfamily{Ii}}}"  

%format  >|<       = "\mathbin{\text{\ttfamily{>|<}}}"
%format  <++>       = "\mathbin{\text{\small\ttfamily{<++>}}}"
%format  +>>        = "\mathbin{\text{\small\ttfamily{+>>}}}"


%include polycode.fmt


\begin{document}


\title{Building an extensible markup converter \\ \small{a tutorial in compositional compiler construction in Haskell}}
\author{Jacco Krijnen}

\maketitle

\begin{abstract}
With the @murder@ and @aspectag@ libraries we can write extensible compilers in Haskell. In this article we will introduce their workings and build a small markup language converter as example.
\end{abstract}

\section{Introduction}
%eerst iets over extensible grammars/compiler construction
% of hierna nog wat over waarom extensibility fijn zou zijn? (verg met pandoc)
Markup languages are diverse and have different purposes, such as blogs, publications and documentation. Yet different situations require different languages: we use latex to write a paper, markdown to document a repository and html to write a webpage. A compiler translates documents in language X to a documents in language Y. And a markup converter is just a special case where X and Y are markup languages. To build such a compiler, we require two things: 

\begin{itemize}
\item{a way of describing the parser for language X. For this we use grammar fragments from the @murder@ library.}
\item{a way of describing the semantics of language Y. For this we we use aspects from the @aspectag@ library.}
\end{itemize}
Both libraries are developed and extensively explained in \cite{Marcos}. This tutorial will serve as an introduction to the usage of the libraries.

We want these two parts to be completely decoupled, so that we can write:

\begin{code}
import Grammars.Latex
import Semantics.Html

latex2html :: String -> String
latex2html input =  let  parser  = compile $ closeGram (gramLatex semHtml)
                          x       = result (parse parser input)
                     in  (x emptyRecord) # output
\end{code}
Thus we require the grammar fragment to accept the semantics as an argument.\\

More important, we would like this tool to be highly extensible, so other developers can define their own plugins and modifications \emph{without having to work with the original source code}, like so:

\begin{code}    
-- support for hyperlinks
gramLatexUrls sem  = ...
semHtmlUrls        = ...

latex2html' :: String -> String
latex2html' input =  let  parser  = compile $ closeGram (gramLatex semHtml +>> gramLatexUrls semHtmlUrls)
                         x       = result (parse parser input)
                    in  (x emptyRecord) # output
\end{code}
Here we see the initial grammar and semantics being extended with |+>>|. Grammar fragments will be explained in section 2, we will use that knowledge to demonstrate a grammar fragment in section 3, aspects will be covered in section 4.

\section{Grammars and the murder library}

\emph{We assume that the reader is familiar with applicative-style parsing and the basics of context-free grammars.}\\

A big difference between @murder@ and conventional parsing libraries is that in @murder@ we write \emph{grammar fragments}: values that represent a context-free grammar, or an extension thereof. Grammar fragments can be composed to build a value of |Grammar a|. It can be optimized, transformed and finally converted into a parser type. @murder@ supports the @uulib@ and the more advanced @uu-parsinglib@ as parser backends. In this article we will use of the latter.  \\


\subsection{A glimpse at a grammar fragment}
As a start we will have a look at a basic grammar fragment defined with the @murder@ EDSL, which we will break down and explain during the following subsections.


\begin{figure}[h]

\begin{code}
$(csLabels ["cs_term","cs_var"])

gram = proc () -> do
    rec  lterm  <-addNT-<  iI semAbs  "\\" var "." lterm    Ii
                   <|>     iI semVar  var                   Ii
                   <|>     iI semApp  lterm " " lterm       Ii
         var    <-addNT-<   iI      (someOf ['a'..'z'])  Ii
    exportNTs-< exportList term  (  export  cs_term  term
                                 .  export  cs_var   var)
semAbs = ...
semVar = ...
semApp = ...
-- the parser
pLambda = compile (closeGram gram)

\end{code}

\caption{A grammar for the lambda calculus}
\end{figure}

The notation might look a bit unfamiliar because of the use of Template Haskell (line 2), arrow syntax (lines 3-9) and the |iI ... Ii| brackets. The |iI| and |Ii| values are known as ``idiom brackets''. They are equivalent with applicative notation and are used because of the resemblance with common CFG notation. Still this is all completely valid Haskell \footnote{using the GHC extensions: Arrows, RecursiveDo, EmptyDataDecls, TemplateHaskell}. The main focus of this section is to make the reader familiar with these concepts and relevant notation. \\

But why write these grammar fragments instead of the plain old parsers? In the first place, the datatypes are more flexible in the sense that it is possible for somebody else to write another fragment that builds on the initial one. It is then possible to retrieve the original productions, make modifications, define new productions, introduce new nonterminals to build an new, extended grammar fragment. As a consequence, @murder@ provides utility functions to perform grammar analysis and optimizations such as left recursion removal and empty productions removal. 

In the second place, @murder@ makes optimal use of the Haskell type and class system to give static guarantees about the grammar fragments. For instance, adding the same nonterminal to a grammar twice results in a compile error. Also, a grammar fragment can only be composed with another fragment that builds upon the first fragment: productions in the new fragment can only refer to nonterminals that are being defined or were defined in the fragment that is being extended (again, this constraint is enforced by the type system).


\subsection{The PreProductions datatype}

On the right hand side of |<-addNT-<| we see an expression of type |PreProductions l env a|. These expressions are the core of the grammar fragment, they describe the alternative productions for the nonterminal being defined. All the other syntax can be seen as the framework in which these definitions exist and will be explained in section 2.4. \\ 

|PreProductions l env| is an applicative functor, and |iI App  lterm " " lterm  Ii| is actually equivalent to |App <$> nt lterm <* tr " " <*> nt lterm|, which should look familiar to parsers. To keep things comprehensive, we will use the applicative notation for now, and discuss the idiom brackets later on.

There are a few primitives that can be used to construct values of type |PreProductions l env a|:

\begin{itemize}
\item |tr :: String -> PreProductions l env (DTerm String)| \\ represents a sequence of terminal symbols. The DTerm type constructor records the positional information in the source file of the recognized terminal.

\item |nt :: Symbol a TNonT env -> PreProductions l env a| \\ refers to a nonterminal, where the argument is a value that is obtained by the pattern match on the left hand side of a |<-addNT-<|. Also, for the domain of programming languages @murder@ has some predefined symbols that can be used, such as |var| for a variable identifier, |op| for an operator symbol and |int| for an integer.

\item |manyOf, someOf, manyExcept, pSomeExcept :: [Char] -> PreProductions l env a| \\ to recognize zero or more and one or more characters of the given characters, or zero or more and one or more except for the given characters.
\end{itemize}

They can be combined with the following combinators:

\begin{itemize}
\item The functions of the alternative and applicative class: |<||>|, pure, |<*>| for alternatives or sequencing.

\item |pMany, pSome :: PreProductions l env a -> PreProductions l env [a]| for recognizing zero or more and one or more productions respectively.

\item |pFoldr :: (a -> b -> b, b) -> PreProductions l env a -> PreProductions l env b| which recogizes zero or more occurences and folds them similar to |foldr|.

\item |pMaybe :: (a -> b, b) -> PreProductions l env a -> PreProductions l env b| which recognizes zero or one occurences and folds them similar to |maybe|.
\end{itemize}

n.b. The types of the last three bullets are a bit simplified in the first two type parameters of |PreProductions|, since these are mainly for internal use in the library.

In short, we can write productions just as we write parser combinators.




\subsection{Idiom brackets}

The idiom brackets allow for syntax that matches common grammar notation. It is equivalent to writing applicative style, but looks quite magical because of the spaces between the function, terminals and nonterminals, which are indeed function application.

Effectively, the application of applicative and lifting of values happen ``under the hood'', and the following rules hold for values that are placed between |iI| and |Ii|:
\begin{itemize}
\item A function to combine all the results is automatically lifted with pure. It is optional to have such a function, otherwise |id| is used
\item A value of type |String| is automatically lifted with |tr| and \emph{ignored} as argument for the function
\item A value of type |Symbol a t env| is automatically lifted with |nt|
\item A value of type |PreProductions l env a| is used without modification
\item values of other types are not accepted (except for some special types defined in the @murder@ library)
\end{itemize}

A well formed value between idiom brackets is again of type |PreProductions l env a|. Note that defining alternatives between the idiom brackets is not possible. If you are happy with these rules and do not care about the inner workings, you can skip the rest of this section. \\

It can be mystifying how such syntax is valid Haskell. For instance, iI is a function that 1. seems to accept a variable amount of arguments (polyvariadic) of different types but is 2. somehow constrained by the type system to accept the arguments in the right order and by the right amount.

Polyvariadic functions can be achieved by function overloading in combination with currying. As an example, we implement a function that concatenates all the |String|s it receives as arguments.

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

class Conc a where
    conc :: String -> a

-- base case
instance Conc String where
    conc = id


-- inductive case
instance (Conc k) => Conc (String -> k) where
    conc xs ys = conc (xs ++ ys)

\end{code}
The two types that are instance of this class are |String| and |Conc k => String -> k|. Thus inductively defining all functions | String -> String -> ... -> String| to be instances. We can now write |conc "a" "b" "c" :: String| . As an exercise, the reader could try to define a more general class and instance such that any type |Show a => a| can be given as argument.

By using the MultiParamTypeClasses and FunctionalDependencies extensions, information about the result being constructed can be taken into account. An implementation for any applicative functor can be found at @http://www.haskell.org/haskellwiki/Idiom_brackets@. The idiom brackets in @murder@ closely resemble this implementation, but are specialized for certain types (as listed above).






\subsection{Grammar fragments and Arrows}

In this subsection we will describe the composition and structure of a grammar fragment, which is the framework around the productions. 

We talk about the \emph{composition} of grammar fragments, a word which is often used for functions. It is interesting to note that extensibility and function composition go hand in hand. A grammar fragment could be thought of as a function that transforms another grammar:

\begin{code}
type GramFragment a b = Grammar a -> Grammar b
(+>>) = (.)
\end{code}

Then we get grammar fragment composition for free, by using function composition. The actual type is more complicated, but it is important to keep this design in mind. \\

In reality, a grammar fragment is an arrow \cite{Arrows}. To be more precise, it is the |Trafo m t s a b| arrow from the @TTTAS@ library \cite{TTTAS}. Arrows are a generalization of functions; just like functions, they receive input, compute output and can be composed. But arrows often carry some extra information or context for the computation. For instance, the |Trafo| arrow is designed to build a heterogeneous list in the background. A heterogeneous list contains values of different types and could be implemented as a nested tuple. To learn more about heterogeneous collections, see the @HList@ library \cite{HList}.  \\

For grammar fragments, this implicit heterogeneous list contains value of |PreProductions l env a|. Note that these values are indeed of different types, since the parameter |a| depends on what is being parsed. By using |addNT| we extend the list, and obtain a value (the left hand side of |<-addNT-<|) that can be used to point into the list at the added productions. \\

But since this heterogeneous list is constructed implicitly, what are the actual input and output types of a grammar fragment? This will be discussed in the next section.

Arrows are an interesting concept, but in the rest of this section we will only offer some analogies to functions, since knowledge of arrow syntax suffices to write grammar fragments. For a more formal introduction, see \cite{ArrowNotation}. To use arrow syntax, the language extension Arrows is required.

Take a look at the arrow equivalent of a lambda function (the code is completely meaningless and only intended to demonstrate the syntax):

\begin{code}
{-# LANGUAGE Arrows, RecursiveDo #-}

arrow0 = proc x -> do
    patternOutput  <-  arrow1-<  "input for arrow1"
    let v = 2 + 3
    more           <-  arrow2-<  (patternOutput, v)
    rec  x         <-  arrow3-<  1:y
         y         <-  arrow3-<  2:x
    arrow4-< "final input"
\end{code}

 |proc x| (arrow abstraction) can be seen as the equivalent of |\x| (lambda abstraction) for functions, while |output <-f-< input| can be seen as an equivalent of |let output = f input in| . Note how, just as in do notation for monads, the last line of an arrow definition should not pattern match on the output, since that will be the arrow's output. Arrows are not really suitable for currying and when an arrow requires multiple arguments, they are therefore given as a tuple. Finally, arrow syntax supports mutually recursive declarations \footnote{for arrows that instantiate the ArrowLoop class} by using the |rec| keyword, analogous to a |let| with multiple declarations.





\subsection{The Export datatype and labels}
The output of a grammar fragment is a value of type |Export a nts env|. A value of this type is expected on the right hand side of |exportNTs-<|. In the above example, we use the utility function |exportList| to construct it. It takes the start nonterminal of the grammar, and a list of exported nonterminals. This datatype is meant as a lookup table for further grammar fragments that will build upon this one. The values prefixed by @cs_@ are created by template haskell with the utility function |csLabels| and are used as keys in the lookuptable. For more detail, see [thesis].

A fragment that extends another fragment should have such an ExportList as arrow input, the type inferencer will find this out. As an example, imagine somebody else who would like to extend the grammar (he/she does not like church encoding):

\begin{code}
$(csLabels ["cs_bool", "cs_int", "cs_str"])

gram' = proc imported -> do
    let lterm = getNT cs_lterm imported
    bool  <-addNT-< ...
    int   <-addNT-< ...
    str   <-addNT-< ...
    addProds-< (lterm, iI semBool bool Ii <|> iI semInt int Ii <|> iI semStr str Ii)
    exportNT-< extendExport imported   (  export cs_bool bool
                                       .  export cs_int  int
                                       .  export cs_str  str)

semBool  = ...
semInt   = ...
semStr   = ...
\end{code}

Here we see the non terminal |lterm| being extended with three alternatives. Furthermore, three new non-terminals are introduced in the grammar. Note |getNT cs_lterm imported|, which gives the type system a hint that |imported| should be an |Export a nts env| containing at least the non-terminal with the label |cs_lterm|.

\subsection{A note about type signatures}
You may have noticed that we don't explicitly write the types of grammar fragments down. Type signatures are often very useful to \emph{document} a value, which is considered to be a good practice. However, for grammar fragment (and attributes as we will see later), the type system is used to \emph{express constraints} about the datatypes in question. Also, heterogeneous lists contain the types of the values in their types. These constraints and types often result in large type signatures that are tedious to write down, and we rely on the type inferencer instead.


\section{Defining the grammar of a markup language}
We can now start with the markup converter and write a grammar for a subset of HTML. We choose to use a small preprocessor that introduces @<plain>@ and @</plain>@ tags to explicitly mark plaintext. This will result in an easier grammar (this could have also been achieved with a scanner).

\begin{code}
$(csLabels  ["cs_document", "cs_blockL", "cs_paragraph", "cs_header", "cs_inline", "cs_inlineL"])


gHtml sem = proc () -> do
    rec 
        document     <-addNT-< iI (pDocument sem) blockL Ii
        
        
        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) "<p>" inlineL "</p>" Ii
        header       <-addNT-< foldr1 (<|>) $ 
                                   map (headerLvl (pHeader sem) inlineL) [1..6]
        
        inline       <-addNT-<  iI (pPlain   sem) "<plain>" (someExcept "<") "</plain>" Ii 
                        <|>     iI (pBold    sem) "<b>"     inlineL          "</b>"     Ii
                        <|>     iI (pItalics sem) "<i>"     inlineL          "</i>"     Ii
        
        inlineL      <-addNT-<  pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                    iI inline Ii
        

    exportNTs -<  exportList document (   export cs_document      document
                                        . export cs_blockL        blockL
                                        . export cs_paragraph     paragraph
                                        . export cs_header        header
                                        . export cs_inline        inline
                                        . export cs_inlineL       inlineL)

where headerLvl pHeader body x =  let  open  = "<h"   ++ show x ++ ">"
                                       close = "</h"  ++ show x ++ ">"
                                  in   iI (pHeader (sem_Lit x)) open body close Ii

\end{code}

Note that the fragment receives a record |sem| with semantic functions (which we will covered in section 4).

\subsection{An extension}
Someone might want to extend the grammar to recognize html hyperlinks, and could then write:

\begin{code}
gHtmlHref sem = proc imported -> do
    let inline = getNT cs_inline imported
    
    _ <-addProds-< (inline,  
            iI (pHref sem) "<a href=\"" (someExcept "\"") "\">" (someExcept "<") "</a>" Ii)
    
    exportNTs -< imported
\end{code}

Again, this fragment receives a record (a different one, containing only this single semantic function).

\subsection{Error correction}

Markup languages like markdown pose a problem: any document is considered to be ``valid'' markdown, since there exists no formal specification. Html documents suffer a similar problem: browsers accept any html document, whether it is correct or not. Can we adapt the markup converter tool to also be this flexible? When using @uu-parsinglib@ as parser backend, we get this functionality for free, since the parsers perform error correction \cite{ErrorParsers}: when a parse is not possible (an expected terminal might be inserted, or tokens from the input might be deleted). This means that whether the document does not completely adhere to our grammar, the parser will correct mistakes and continue parsing.\\

For example, consider a markup converter that converts from html to html:

\begin{code}
import Grammars.Html
import Semantics.Html

html2html = buildConverter (gramHtml semHtml)

\end{code}

Then the following input:

\begin{verbatim}
<p><b>this is bold? <i>or italics!</i></p>
This text should be between p tags
<h1>is this h1 or h6?</h6>
\end{verbatim}


results in:
\begin{verbatim}
<p><b>this is bold? <i>or italics!</i></b></p>
<p>This text should be between p tags</p>
<h1>is this h1 or h6?</h1><h6></h6>
\end{verbatim}


%% GRAMMAR EXTENSIONS
%%



\section{Semantics with aspectag}

Now that we can write grammar descriptions, we would also like to define semantic functions. In the case of our markup converter, the semantics is a string in the target markup language. We use the @aspectag@ library which is built around the notion of attribute grammars. It provides a modular mechanism with which one can define groups of attributes, called \emph{aspects}. In this section we will start by explaining the relevant terminology, then explain 

\subsection{Abstract syntax and trees}
The most common way to express abstract syntax (AS) in Haskell is to define datatypes that correspond to nonterminals and dataconstructors that correspond to productions (when applied to their values). From now on, both correspondences will be used as synonyms when talking about AS.

For our markup converter, we will use the following AS:

\begin{code}

data Document = Document { blocks       :: BlockL }
    deriving Show


type BlockL = [Block]

data Block  =  Header    { level_header    :: Int,
                           inlines_header  :: InlineL }
            |  Paragraph { inlines_par     :: InlineL }
            deriving (Show)


type InlineL = [Inline]

data Inline  =  Plain   { str_plainInl     :: String }
             |  Bold    { inlines_boldInl  :: InlineL }
             |  Italics { inlines_italInl  :: InlineL }
             deriving (Show)       

\end{code}

This represents a very basic markup document in a general form. The explicit type synonyms are required for @aspectag@. \\

A value of type |Document| can be seen as a tree (or a parse tree when resulting from a parser). We can then call a dataconstructor a \emph{node}, and its values \emph{child nodes}. Attribute grammars [ref] are a formalism to describe computations through such a tree.





\subsection{Folds and attributes}
A fold over the abstract syntax requires for every datatype |T| an \emph{algebra}: a function for every constructor of |T| that combines the results of recursively folding its children into a result value. These functions are known as semantic functions. Extra values can be threaded down the datatype by having a function as result of the fold: the parameters are filled in by the parent node during the folding process.

When inspecting such a semantic function, we can recognize two different kind of values:

\begin{enumerate}
\item{The result value of the function that will be used by the parent node in the folding process. Examples of such values for compilers are: the output in the target language (code generation), a list of errors from the typechecker, a symbol table being constructed.}
\item{values that are passed down from the parent node as argument of the result function. In compilers, such values might include the completed symbol table (for typechecking/name analysis).}
\end{enumerate}

From now on, we will call the first kind \emph{synthesized attributes} and the second one \emph{inherited attributes}. \\

In our markup converter, it looks like we only need a single synthesized attribute, i.e. a |String| in the target markup language. For example, we could have specified Html semantics for the Inlines part of our Document datatype as:

\begin{figure}[h]
\begin{code}
fold :: InlinesAlgebra inls inl -> Inlines -> inls
fold = ...

data InlinesAlgebra inls inl = InlinesAlgebra {
   pInlineCons  :: inl -> inls -> inls,
   pInlineNil   :: inls,

   pPlain       :: String  -> inl,
   pBold        :: inls    -> inl,
   pItalics     :: inls    -> inl
}

htmlSem :: InlinesAlgebra String String
htmlSem = InlinesAlgebra { 
    pInlineCons  = \inl inls ->  inl ++ inls,
    pInlineNil   =  "",

    pPlain       = \str  -> str,
    pBold        = \inls -> "<b>" ++ inls ++ "</b>",
    pItalics     = \inls -> "<i>" ++ inls ++ "</i>"
}

\end{code}
\caption{Html semantics for Inlines}
\end{figure}
\emph{(in all of the following examples, we will only work with the |InlineL| and |Inline| types, instead of the complete Document type, please see the package for the complete code)}\\

Thus the semantic functions are stored in a record (we have seen the use of such a record in the grammar fragment [ref]). \\

However, an algebra like |htmlSem| is not very extensible. For example, there is no real way to compute another synthesized attribute or thread down an inherited attribute in the same fold other than changing all the semantic functions in the original source code. @aspectag@ introduces an EDSL that solves this problem. With it we define \emph{rules} for computing an attribute at a dataconstructor (production). Similar to a semantic function, we have access to the synthesized attributes of the children and the inherited attributes of the parent. The big difference is that rules are extensible. \\

Just like @murder@ can derive the parsers from grammar fragment, @aspectag@ can derive a record with semantic functions from attribute rules. This will be demonstrated in section [ref].



\subsection{Rules}
We will now show how to express the same semantics with @aspectag@, and explain the types and syntax.


% language extensions?
\begin{code}
$(deriveAG ''Document)
$(attLabels ["shtml"])

inlineLnil_shtml   = syn shtml $ return ""
inlineLcons_shtml  = syn shtml $
    do  inl   <- at ch_hd_InlineL_Cons
        inls  <- at ch_tl_InlineL_Cons
        return $ inl # shtml ++ inls # shtml


plain_shtml        = syn shtml $ liftM id (at ch_str_plainInl)

bold_shtml         = syn shtml $ 
    do  inls <- at ch_inlines_boldInl
        return $ "<b>" ++ inls # shtml ++ "</b>"

italics_shtml      = syn shtml $ 
    do  inls <- at ch_inlines_italInl
        return $ "<i>" ++ inls # shtml ++ "</i>"
\end{code}

The first line of template haskell creates a lot of boilerplate code. For example, for every field of the dataconstructors, a label is created. We define rules for the |shtml| attribute (synthesized html) by using the |syn| function. |syn| takes the name (a label) of the attribute that we are defining and a value of the Reader monad. In it, we can access the child nodes with |child <- at ch_label|, then we can access their attributes with |child # attribute|. \\

We can now generate the record of semantic functions with:

\begin{code}
$(deriveLang "Inl" [InlineL, ''Inline])

aspInlineL_Nil  = inlineLnil_output
aspInlineL_Cons = inlineLcons_output
aspPlain        = plain_output
aspBold         = bold_output
aspItalics      = italics_output

semHtml = mkInl aspBold aspInlineL_Cons aspInlineL_Nil aspItalics aspParagraph aspPlain
\end{code}

|deriveLang| generates the type of the semantic record, and the function mkInl, which maps all the rules onto such a semantic function.

\subsection{Extending the semantics}
We were able to extend a grammar with extra productions (for example to recognize hyperlinks). We would now also like to do the same for the semantics: add a rule for generating html. However, there is no abstract syntax that describes a hyperlink! It should be a constructor of the |Inline| type, but it would defeat the purpose if we had to edit the source code of the original abstract syntax, call |deriveLang| again etc.

Therefore, @aspectag@ allow us to define a seperate datatype, like so:

\begin{code}
data EXT_Inline = Href { href_address :: String, href_description :: String }

$(extendAG ''EXT_Inline [ ])
$(deriveLang "DocHref" [''EXT_Inline])
\end{code}





\begin{thebibliography}{1}

\bibitem{Marcos} Viera, M. First Class Syntax, Semantics, and Their Composition, PhD Thesis, Utrecht University - PEDECIBA, 2013 .

\bibitem{Arrows} John Hughes, Generalising Monads to Arrows, in Science of Computer Programming 37, pp67-111, May 2000. 

\bibitem{TTTAS} Arthur I. Baars, S. Doaitse Swierstra, and Marcos Viera. Typed Transformations of Typed Abstract Syntax. In TLDI '09: fourth ACM SIGPLAN Workshop on Types in Language Design and Implementation, 15-26, New York, NY, USA, 2009. ACM. 

\bibitem{HList} Kiselyov, Oleg, Ralf Lämmel, and Keean Schupke. "Strongly typed heterogeneous collections." Proceedings of the 2004 ACM SIGPLAN workshop on Haskell. ACM, 2004.

\bibitem{ArrowNotation} Ross Paterson. A new notation for arrows. International Conference on Functional Programming, Firenze, Italy, 3-5 September 2001, 229-240. 

 
\bibitem{ErrorParsers} Swierstra, S. Doaitse, and Luc Duponcheel. Deterministic, error-correcting combinator parsers. Springer Berlin Heidelberg, 1996.

\end{thebibliography}

\end{document}