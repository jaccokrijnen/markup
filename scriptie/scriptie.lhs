\documentclass{article}

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
Markup languages are diverse and have different purposes, such as blogs, publications and documentation. Yet different situations require different languages: we use latex to write a paper, markdown to document a repository and html to write a webpage. A markup converter is a tool that compiles a document in language X to a document in language Y, where X and Y are markup languages. To accomplish this, we require two things: 

\begin{itemize}
\item{a way of describing the parser for language X. For this we use grammar fragments from the @murder@ library.}
\item{a way of describing the semantics of language Y. For this we we use aspects from the @aspectag@ library.}
\end{itemize}
We want these two parts to be completely decoupled, so that we can write:

\begin{code}
import Grammars.Latex
import Semantics.Html

latex2html :: String -> (String, [Message])
latex2html = buildConverter (gramLatex semHtml)
\end{code}
Thus we require the grammar fragment to accept the semantics as an argument.\\

Also, we would like this tool to be highly extensible, so other developers can define their own plugins and modifications, like so:

\begin{code}    
-- support for hyperlinks
gramLatexUrls sem  = ...
semHtmlUrls        = ...

latex2html' = buildConverter  (gramLatex semHtml +>> gramLatexUrls semHtmlUrls) 
\end{code}
Here we see the initial grammar and initial semantics being extended with +>>. Grammar fragments will be explained in section [], aspects will be covered in section []. 

% example met aanpassing?

% opbouw uitleg: inleiding tot murder, aspectag en usecase (of murder-usecase grams, aspectag usecase-sem)
\section{Grammars and the murder library}

\emph{We assume that the reader is familiar with applicative-style parsing and the basics of context-free grammars.}\\

A big difference between @murder@ and conventional parsing libraries is that we write \emph{grammar fragments}. We can compose several grammar fragment into a |Grammar a|, optimize and transform it, and finally convert it into a parser type. @murder@ supports the @uulib@ and the more advanced @uu-parsinglib@ as parser backends. In this article we will make use of the latter. A grammar fragment outputs a collection of pairs (non-terminal, productions) together with a start non-terminal, similar to a context-free grammar. \\


\emph{A glimpse at a grammar fragment}
As a start we will have a look at a basic grammar fragment defined with the @murder@ EDSL, which we will break down and explain during the following subsections.

\begin{code}

data LTerm = Abs String LTerm | Var String | App LTerm LTerm

$(cs_labels ["cs_term","cs_var"])

gram = proc () -> do
    rec  term  <-addNT-<  iI Abs  "\\" lVar "." lTerm   Ii
                  <|>     iI Var  var                  Ii
                  <|>     iI App  lterm " " lterm      Ii
         var   <-addNT-<  iI    ( someOf ['a'..'z'])  Ii
    exportNTs-< exportList term  (  export  cs_term  term
                                 .  export  cs_var   var)

-- the parser
pLambda = compile (closeGram gram)

\end{code}
The notation might look a bit unfamiliar because of the arrow syntax and the |iI ... Ii| brackets. The |iI| and |Ii| values are known as ``idiom brackets''. They are equivalent with applicative notation and are used because of the resemblance with common CFG notation. Still this is all still completely valid Haskell.

But why write these grammar fragments instead of the plain old parsers? In the first place, this datatype is more flexible since it is designed as a deeply embedded DSL using typed abstract syntax. @murder@ depends on the @TTTAS@ package to allow for transformations on this AS, while keeping the [ref]. It is flexible in the sense that it is possible for somebody else to import it, retrieve the individual productions, make modifications, define new productions, introduce new non-terminals to build an new, extended grammar. As a consequence, murder can also perform grammar analysis and optimizations such as left recursion removal and empty productions removal. 

In the second place, @murder@ makes optimal use of the Haskell type and class system to give static guarantees about the grammar fragments. For instance, adding the same non-terminal to a grammar twice results in a compile error. Also, the composition of two grammar fragments can only happen in valid ways: productions can only refer to non-terminals that are being defined or were defined in the fragments that is being extended.  

% [voorbeeld met extension? bv numerals ipv onhandige church numerals]

\subsection{The PreProductions datatype}

On the right hand side of |<-addNT-<| we see a value of type |PreProductions l env a| that describes the alternative productions for the non-terminal being defined. |PreProductions l env| is an applicative functor, and |iI Email local "@" domain Ii| is equivalent to |Email <$> nt sym <* tr "@" <*> nt domain|. To keep things comprehensive, we will use the applicative style only in this section, and discuss the idiom brackets later on.
% miss die vertaling pas na introductie combinators?

There are a few primitives that can be used to construct values of type |PreProductions l env a|:

\begin{itemize}
\item |tr :: String -> PreProductions l env (DTerm String)| \\ represents a sequence of terminal symbols. The DTerm type constructor records the positional information in the source file of the recognized terminal.

\item |nt :: Symbol a TNonT env -> PreProductions l env a| \\ refers to a non-terminal, where the argument is a value that is obtained by the pattern match on the left hand side of a |<-addNT-<|. Also, for the domain of programming languages @murder@ has some predefined symbols that can be used, such as |var| for a variable identifier, |op| for an operator symbol and |int| for an integer.

\item |manyOf, someOf, manyExcept, pSomeExcept :: [Char] -> PreProductions l env a| \\ to recognize zero or more and one or more characters of the given characters, or zero or more and one or more except for the given characters.
\end{itemize}

They can be combined with the following combinators:

\begin{itemize}
\item The functions of the alternative and applicative class: |<||>|, pure, |<*>| for alternatives or sequencing.

\item |pMany, pSome :: PreProductions l env a -> PreProductions l env [a]| for recognizing zero or more and one or more productions respectively.

\item |pFoldr :: (a -> b -> b, b) -> PreProductions l env a -> PreProductions l env b| which recogizes zero or more occurences and folds them similar to |foldr|.

\item |pMaybe :: (a -> b, b)| which recognizes zero or one occurences and folds them similar to |fromMaybe|.
\end{itemize}

n.b. The types of the last three bullets are a bit simplified in the first two type parameters of |PreProductions|, since these are mainly for internal use in the library.

For example, we could write the following to express two alternatives for recognizing 

\subsection{Idiomatic brackets}

The idiom brackets [bron haskell.org] allow for syntax that matches common grammar notation. It is equivalent to writing applicative style, but looks quite magical because of the spaces between the function, terminals and non-terminals, which are indeed function application.

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
class Conc a where
    conc :: String -> a

-- base case
instance Conc String where
    conc = id


-- inductive case
instance (Conc k) => Conc (String -> k) where
    conc xs ys = conc (xs ++ ys)

\end{code}
The two types that are instance of this class are |String| and |Conc k => String -> k|. Thus inductively defining all functions | String -> String -> ... -> String| to be instances. We can now write |conc "polyvariadic" " " "function"| . As an exercise, the reader could try to define a more general class and instance such that any type |Show a => a| can be given as argument.

By using the MultiParamTypeClasses and FunctionalDependencies extensions, information about the result being constructed can be taken into account. In [bron haskell.org], an implementation is given for any applicative type constructor. The idiom brackets in @murder@ closely resemble this implementation, but are specialized for certain types (as listed above).

\subsection{ExportList datatype and labels}
On the right hand side of |exportNTs-<| a value of type |ExportList a nts env| is required. In the above example, we use the utility function |exportList| which takes the start non-terminal of the grammar, and a list of exported non-terminals. We say ``exported non-terminals'' to indicate that these non-terminals can be modified by later grammar fragments that extend this fragment. The type parameters of |ExportList| indicatie the type of the 

The |export| functions takes a label and a non-terminal symbol. These labels are used as index in a heterogenous list with the non-terminals


\subsection{Grammar fragments and Arrows}

We talk about the \emph{composition} of grammar fragments, although this term is most commonly used for functions. It is interesting to note that extensibility and function composition go hand in hand. A grammar fragment could be thought of as a function that transforms another grammar:

\begin{code}
type GramFragment a b = Grammar a -> Grammar b
(+>>) = (.)
\end{code}

Then we get grammar fragment composition for free, by using function composition. The actual type is more complicated, but it is important to keep this design in mind.

In reality, grammar extensions are Arrows[ref], a generalization of functions. Just like functions, they receive input and produce output and can be composed. The types contain information about which non-terminals the fragment exports.
Arrows are an interesting concept, but in the rest of this section we will only offer some analogies to functions, since knowledge of arrow syntax suffices to write grammar fragments. For a more formal introduction, see [...]. 

Take a look at the arrow equivalent of a lambda function:

\begin{code}
arrow0 = proc x -> do
    patternOutput  <-  arrow1-<  "input for arrow1"
    more           <-  arrow2-<  (3, patternOutput)
                       arrow1-< "more"
\end{code}

|proc x| (arrow abstraction) can be seen as the equivalent of |\x| (lambda abstraction) for functions, while |output <-f-< input| can be seen as an equivalent of |let output = f input in| . Note how the last line of an arrow definition should not pattern match on the output, since that will be the arrow's output. Arrows are not really suitable for currying and when an arrow requires multiple arguments, they are therefore given as a tuple.

Finally, arrow syntax supports mutually recursive declarations \footnote{for arrows that instantiate the ArrowLoop class} by using the rec keyword, analogous to a |let| with multiple declarations.


\subsection{A note about type signatures}
You may have noticed that we don't explicitly write the types of grammar fragments down. Type signatures are often very useful to \emph{document} a value, which is considered to be a good practice. However, for grammar fragment (and attributes as we will see later), the type system is used to \emph{express constraints} about the datatypes in question. For instance, [grammar fragment requires non-terminals to extend] . These constraints often result in large type signatures that are tedious to write down, and we rely on the type inferencer instead.


\section{Defining the grammar of a markup language}
We can now start with the markup converter and write a grammar for HTML:

\begin{code}
-- Generate the labels used as lookup keys in the exportlist
$(csLabels  ["cs_document", "cs_blockL", "cs_paragraph", "cs_header", "cs_inline", "cs_inlineL"])


gHtml sem = proc () -> do
    rec 
        document     <-addNT-< iI (pDocument sem) blockL Ii
        
        
        blockL       <-addNT-< pFoldr (pBlockL_Cons sem, pBlockL_Nil sem) $ 
                                   (iI header Ii) <|> (iI paragraph Ii)
        paragraph    <-addNT-< iI (pParagraph sem) "<p>" inlineL "</p>" Ii
        header       <-addNT-< foldr1 (<|>) $ 
                                   map (headerLvl (pHeader sem) inlineL) [1..6]
        
        -- this seperation is required for the inlines non-terminal
        inline       <-addNT-<  iI (pPlain   sem) "<plain>" (someExcept "<") "</plain>" Ii 
                        <|>     iI (pBold    sem) "<b>"     inlineL          "</b>"     Ii
                        <|>     iI (pItalics sem) "<i>"     inlineL          "</i>"     Ii
        
        -- Multiple inlines
        inlineL      <-addNT-<  pFoldr (pInlineL_Cons sem, pInlineL_Nil sem) $
                                    iI inline Ii
        

        


    exportNTs -<  exportList document (   export cs_document      document
                                        . export cs_blockL        blockL
                                        . export cs_paragraph     paragraph
                                        . export cs_header        header
                                        . export cs_inline        inline
                                        . export cs_inlineL       inlineL)

\end{code}






\section{Semantics with aspectag}

Now that we can write grammar descriptions, we would like to define semantic functions







\end{document}