NB. NB. ============================================================
NB. NB. ex4.ijs - Jack Tokenizer (nand2tetris Project 10, Stage 1)
NB. NB. ============================================================
NB. NB. Usage (from jconsole):
NB. NB.   load 'ex4.ijs'
NB. NB.   Main '/path/to/SomeFile.jack'
NB. NB.   Main '/path/to/SomeFolder'
NB. NB. ============================================================


NB. NB. ------- Global constants -------

NB. NB. Jack keywords as boxed list
NB. KEYWORDS =: ;: 'class constructor function method field static var int char boolean void true false null this let do if else while return'

NB. NB. Jack symbol characters (each is a single-token symbol)
NB. SYMBOLS =: '{}()[].,;+-*/&|<>=~'

NB. NB. Whitespace: space, tab(9), LF(10), CR(13)
NB. WS =: ' ' , (a.{~9) , (a.{~10) , (a.{~13)

NB. NB. All letter characters a-z and A-Z
NB. LETTERS =: (a.{~(a.i.'a')+i.26) , (a.{~(a.i.'A')+i.26)

NB. NB. Digit characters
NB. DIGITS =: '0123456789'

NB. NB. ------- File I/O -------

NB. NB. ReadFile 'path' -- reads file as flat character vector
NB. ReadFile =: 1!:1 @ boxopen

NB. NB. 'content' WriteFile 'path' -- writes string to file
NB. WriteFile =: 4 : 0
NB.   x 1!:2 < y
NB. )
NB. NB. ------- Character predicates -------

NB. NB. IsDigit c -- 1 iff c is a digit character
NB. IsDigit =: 3 : 0
NB.   y e. DIGITS
NB. )

NB. NB. IsLetter c -- 1 iff c is a-z or A-Z
NB. IsLetter =: 3 : 0
NB.   y e. LETTERS
NB. )
NB. NB. IsIdentifierChar c -- 1 iff c can appear inside an identifier

NB. IsIdentifierChar =: 3 : 0
NB.   (IsLetter y) +. (IsDigit y) +. y = '_'
NB. )

NB. NB. IsSpace c -- 1 iff c is whitespace
NB. IsSpace =: 3 : 0
NB.   y e. WS
NB. )
NB. NB. ------- XML escaping -------

NB. NB. EscapeXml 'str' -- replaces <, >, &, " with XML entities
NB. EscapeXml =: 3 : 0
NB.   r =. ''
NB.   for_c. y do.
NB.     select. c
NB.       case. '<' do. r =. r , '&lt;'
NB.       case. '>' do. r =. r , '&gt;'
NB.       case. '"' do. r =. r , '&quot;'
NB.       case. '&' do. r =. r , '&amp;'
NB.       case.      do. r =. r , c
NB.     end.
NB.   end.
NB.   r
NB. )

NB. NB. ------- Tokenizer -------

NB. NB. Tokenize 'src' -- given Jack source text, return boxed list of (type;value) pairs
NB. Tokenize =: 3 : 0
NB.   src   =. y
NB.   n     =. # src
NB.   i     =. 0
NB.   tlist =. 0 # a:   NB. empty boxed list to accumulate tokens
NB.   LF    =. a. {~ 10

NB.   while. i < n do.
NB.     c =. i { src

NB.     NB. ---- Skip whitespace ----
NB.     if. IsSpace c do.
NB.       i =. i + 1
NB.       continue.
NB.     end.

NB.     NB. ---- Comments ----
NB.     if. (c = '/') *. (i + 1 < n) do.
NB.       nc =. (i + 1) { src

NB.       NB. Line comment: // to end of line
NB.       if. nc = '/' do.
NB.         i =. i + 2
NB.         while. (i < n) *. (LF ~: i { src) do. i =. i + 1 end.
NB.         continue.
NB.       end.

NB.       NB. Block comment: /* ... */ (includes doc comments /** ... */)
NB.       if. nc = '*' do.
NB.         i =. i + 2
NB.         closed =. 0
NB.         while. (i + 1 < n) *. -. closed do.
NB.           if. ('*' = i { src) *. ('/' = (i + 1) { src) do.
NB.             i      =. i + 2
NB.             closed =. 1
NB.           else.
NB.             i =. i + 1
NB.           end.
NB.         end.
NB.         if. -. closed do. i =. n end.  NB. safety: skip to end if unclosed
NB.         continue.
NB.       end.
NB.     end.

NB.     NB. ---- String constant: "..." ----
NB.     if. c = '"' do.
NB.       i =. i + 1           NB. skip opening double-quote
NB.       j =. i
NB.       while. (j < n) *. ('"' ~: j { src) do. j =. j + 1 end.
NB.       val   =. (j - i) {. i }. src   NB. content between quotes
NB.       tlist =. tlist , < ('stringConstant' ; val)
NB.       i     =. j + 1                  NB. skip closing double-quote
NB.       continue.
NB.     end.

NB.     NB. ---- Integer constant: digit sequence ----
NB.     if. IsDigit c do.
NB.       j =. i + 1
NB.       while. (j < n) *. IsDigit (j { src) do. j =. j + 1 end.
NB.       val   =. (j - i) {. i }. src
NB.       tlist =. tlist , < ('integerConstant' ; val)
NB.       i     =. j
NB.       continue.
NB.     end.

NB.     NB. ---- Symbol: single-character token ----
NB.     if. c e. SYMBOLS do.
NB.       tlist =. tlist , < ('symbol' ; (1 {. i }. src))
NB.       i     =. i + 1
NB.       continue.
NB.     end.

NB.     NB. ---- Keyword or Identifier: starts with letter or underscore ----
NB.     if. (IsLetter c) +. (c = '_') do.
NB.       j =. i + 1
NB.       while. (j < n) *. IsIdentifierChar (j { src) do. j =. j + 1 end.
NB.       val =. (j - i) {. i }. src
NB.       NB. Check if the word is a reserved keyword
NB.       if. (<val) e. KEYWORDS do.
NB.         tlist =. tlist , < ('keyword' ; val)
NB.       else.
NB.         tlist =. tlist , < ('identifier' ; val)
NB.       end.
NB.       i =. j
NB.       continue.
NB.     end.

NB.     NB. ---- Unknown/unexpected character: skip silently ----
NB.     i =. i + 1
NB.   end.

NB.   tlist
NB. )

NB. NB. ------- Token -> XML line -------

NB. NB. TokenToXml tok -- tok is a box holding (type ; value)
NB. NB. Produces:  <type> escaped_value </type>
NB. TokenToXml =: 3 : 0
NB.   typ     =. > 0 { y
NB.   val     =. > 1 { y
NB.   escaped =. EscapeXml val
NB.   '<' , typ , '> ' , escaped , ' </' , typ , '>'
NB. )

NB. NB. ------- Analyze a single .jack file -------

NB. NB. AnalyzeFile 'path/to/Xxx.jack' -- writes 'path/to/XxxT.xml'
NB. AnalyzeFile =: 3 : 0
NB.   path =. y
NB.   LF   =. a. {~ 10

NB.   NB. Build output path: strip '.jack', append 'T.xml'
NB.   NB. Find position of last '.' in path
NB.   dotpos =. <: # path    NB. default fallback
NB.   k      =. <: # path
NB.   while. k >: 0 do.
NB.     if. '.' = k { path do.
NB.       dotpos =. k
NB.       k      =. _1        NB. signal loop exit
NB.     else.
NB.       k =. k - 1
NB.     end.
NB.   end.
NB.   base    =. dotpos {. path       NB. path without extension
NB.   outpath =. base , 'T.xml'

NB.   NB. Read source, tokenize, format XML
NB.   src   =. ReadFile path
NB.   tlist =. Tokenize src

NB.   xml =. '<tokens>' , LF
NB.   for_tok. tlist do.
NB.     xml =. xml , (TokenToXml > tok) , LF
NB.   end.
NB.   xml =. xml , '</tokens>' , LF

NB.   NB. Write output
NB.   xml WriteFile outpath
NB.   smoutput 'Written: ' , outpath
NB. )

NB. NB. ------- Directory listing: get .jack files from a folder -------
NB. NB.for Hagit
NB. NB. GetJackFiles 'folder' -- returns boxed list of full paths to .jack files
NB. GetJackFiles =: 3 : 0
NB.   folder =. y
NB.   LF =. a. {~ 10

NB.   if. -. ({: folder) e. '/\' do.
NB.     folder =. folder , '/'
NB.   end.

NB.  raw =. shell 'ls "' , folder , '"*.jack 2>/dev/null'

NB.   if. 0 = # raw do.
NB.     0 # a:
NB.   else.
NB.     files =. <;._2 raw , LF
NB.     files =. files #~ 0 < #&> files
NB.     files
NB.   end.
NB. )

NB. NB.for Ravid
NB. NB. GetJackFiles =: 3 : 0
NB. NB.   dir =. y
NB. NB.   if. '/' = {: dir do.
NB. NB.     dir =. }: dir
NB. NB.   end.
NB. NB.   files =. 1!:0 < dir , '/*.jack'
NB. NB.   if. 0 = # files do.
NB. NB.     0 # a:
NB. NB.   else.
NB. NB.     names =. {."1 files
NB. NB.     (< dir , '/') ,&.> names
NB. NB.   end.
NB. NB. )

NB. NB. ------- Main -------

NB. NB. Main 'path' -- path is either Xxx.jack or a folder
NB. Main =: 3 : 0
NB.   path =. y

NB.   NB. Single-file mode: path ends with '.jack'
NB.   if. '.jack' -: (_5) {. path do.
NB.     AnalyzeFile path
NB.     return.
NB.   end.

NB.   NB. Folder mode: process every .jack file in the folder
NB.   jackfiles =. GetJackFiles path
NB.   if. 0 = # jackfiles do.
NB.     smoutput 'No .jack files found in: ' , path
NB.     return.
NB.   end.
NB.   for_f. jackfiles do.
NB.     AnalyzeFile > f
NB.   end.
NB. )

NB. NB. Startup message
NB. smoutput 'ex4.ijs loaded. Jack Tokenizer ready.'
NB. smoutput '  Single file:  Main ''/path/to/Xxx.jack'''
NB. smoutput '  Folder:       Main ''/path/to/Folder'''


NB. NB. ============================================================
NB. NB. Part B parser.ijs - Jack Parser / Compilation Engine
NB. NB. ============================================================


NB. LF =: a. {~ 10

NB. NB. ------- Parser state -------

NB. tokens_list =: 0 # a:
NB. pointer =: 0

NB. NB. Create spaces for XML indentation
NB. Spaces =: 3 : 0
NB.   y # ' '
NB. )

NB. NB. Create one XML line with indentation
NB. TagLine =: 4 : 0
NB.   (Spaces x) , y , LF
NB. )

NB. NB. Get token by index
NB. GetToken =: 3 : 0
NB.   if. y < # tokens_list do.
NB.     > y { tokens_list
NB.   else.
NB.     ''
NB.   end.
NB. )

NB. NB. Get current token
NB. CurrentToken =: 3 : 0
NB.   GetToken pointer
NB. )

NB. NB. Move to next token
NB. MoveNext =: 3 : 0
NB.   pointer =: pointer + 1
NB. )

NB. NB. Write current token and advance
NB. Consume =: 4 : 0
NB.   tok =. CurrentToken ''
NB.   MoveNext ''
NB.   (Spaces x) , tok , LF
NB. )

NB. NB. Check if current token contains a specific value
NB. TokenIs =: 3 : 0
NB.   1 e. (' ' , y , ' ') E. CurrentToken ''
NB. )

NB. NB. Check next token value
NB. NextTokenIs =: 3 : 0
NB.   1 e. (' ' , y , ' ') E. GetToken pointer + 1
NB. )

NB. NB. Check if current token contains text
NB. CurrentHas =: 3 : 0
NB.   1 e. y E. CurrentToken ''
NB. )

NB. NB. Check identifier token
NB. IsIdentifier =: 3 : 0
NB.   CurrentHas '<identifier>'
NB. )

NB. NB. Check expression operator
NB. IsOp =: 3 : 0
NB.   (TokenIs '+') +. (TokenIs '-') +. (TokenIs '*') +. (TokenIs '/') +. (TokenIs '&amp;') +. (TokenIs '|') +. (TokenIs '&lt;') +. (TokenIs '&gt;') +. (TokenIs '=')
NB. )

NB. NB. ------- Load tokens from XxxT.xml -------

NB. ReadParserTokens =: 3 : 0
NB.   lines =. <;._2 ReadFile y
NB.   lines =. dltb each lines
NB.   lines =. lines -. <''
NB.   lines =. lines -. <'<tokens>'
NB.   lines =. lines -. <'</tokens>'
NB.   lines
NB. )

NB. NB. ============================================================
NB. NB. class
NB. NB. ============================================================

NB. CompileClass =: 3 : 0
NB.   tokens_list =: y
NB.   pointer =: 0

NB.   out =. '<class>' , LF

NB.   out =. out , 2 Consume ''  NB. class
NB.   out =. out , 2 Consume ''  NB. className
NB.   out =. out , 2 Consume ''  NB. {

NB.   while. (TokenIs 'static') +. (TokenIs 'field') do.
NB.     out =. out , CompileClassVarDec ''
NB.   end.

NB.   while. (TokenIs 'constructor') +. (TokenIs 'function') +. (TokenIs 'method') do.
NB.     out =. out , CompileSubroutine ''
NB.   end.

NB.   out =. out , 2 Consume ''  NB. }
NB.   out =. out , '</class>' , LF

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. classVarDec
NB. NB. ============================================================

NB. CompileClassVarDec =: 3 : 0
NB.   out =. 2 TagLine '<classVarDec>'

NB.   out =. out , 4 Consume ''  NB. static / field
NB.   out =. out , 4 Consume ''  NB. type
NB.   out =. out , 4 Consume ''  NB. varName

NB.   while. TokenIs ',' do.
NB.     out =. out , 4 Consume ''  NB. ,
NB.     out =. out , 4 Consume ''  NB. varName
NB.   end.

NB.   out =. out , 4 Consume ''  NB. ;
NB.   out =. out , 2 TagLine '</classVarDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. subroutineDec
NB. NB. ============================================================

NB. CompileSubroutine =: 3 : 0
NB.   out =. 2 TagLine '<subroutineDec>'

NB.   out =. out , 4 Consume ''  NB. constructor / function / method
NB.   out =. out , 4 Consume ''  NB. return type
NB.   out =. out , 4 Consume ''  NB. subroutineName
NB.   out =. out , 4 Consume ''  NB. (

NB.   out =. out , CompileParameterList 4

NB.   out =. out , 4 Consume ''  NB. )

NB.   out =. out , CompileSubroutineBody 4

NB.   out =. out , 2 TagLine '</subroutineDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. parameterList
NB. NB. ============================================================

NB. CompileParameterList =: 3 : 0
NB.   out =. y TagLine '<parameterList>'

NB.   if. -. TokenIs ')' do.
NB.     out =. out , (y + 2) Consume ''  NB. type
NB.     out =. out , (y + 2) Consume ''  NB. varName

NB.     while. TokenIs ',' do.
NB.       out =. out , (y + 2) Consume ''  NB. ,
NB.       out =. out , (y + 2) Consume ''  NB. type
NB.       out =. out , (y + 2) Consume ''  NB. varName
NB.     end.
NB.   end.

NB.   out =. out , y TagLine '</parameterList>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. subroutineBody
NB. NB. ============================================================

NB. CompileSubroutineBody =: 3 : 0
NB.   out =. y TagLine '<subroutineBody>'

NB.   out =. out , (y + 2) Consume ''  NB. {

NB.   while. TokenIs 'var' do.
NB.     out =. out , CompileVarDec (y + 2)
NB.   end.

NB.   out =. out , CompileStatements (y + 2)

NB.   out =. out , (y + 2) Consume ''  NB. }

NB.   out =. out , y TagLine '</subroutineBody>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. varDec
NB. NB. ============================================================

NB. CompileVarDec =: 3 : 0
NB.   out =. y TagLine '<varDec>'

NB.   out =. out , (y + 2) Consume ''  NB. var
NB.   out =. out , (y + 2) Consume ''  NB. type
NB.   out =. out , (y + 2) Consume ''  NB. varName

NB.   while. TokenIs ',' do.
NB.     out =. out , (y + 2) Consume ''  NB. ,
NB.     out =. out , (y + 2) Consume ''  NB. varName
NB.   end.

NB.   out =. out , (y + 2) Consume ''  NB. ;
NB.   out =. out , y TagLine '</varDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. statements
NB. NB. ============================================================

NB. CompileStatements =: 3 : 0
NB.   out =. y TagLine '<statements>'

NB.   while. (TokenIs 'let') +. (TokenIs 'if') +. (TokenIs 'while') +. (TokenIs 'do') +. (TokenIs 'return') do.

NB.     if. TokenIs 'let' do.
NB.       out =. out , CompileLet (y + 2)

NB.     elseif. TokenIs 'if' do.
NB.       out =. out , CompileIf (y + 2)

NB.     elseif. TokenIs 'while' do.
NB.       out =. out , CompileWhile (y + 2)

NB.     elseif. TokenIs 'do' do.
NB.       out =. out , CompileDo (y + 2)

NB.     elseif. TokenIs 'return' do.
NB.       out =. out , CompileReturn (y + 2)

NB.     end.
NB.   end.

NB.   out =. out , y TagLine '</statements>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. letStatement
NB. NB. ============================================================

NB. CompileLet =: 3 : 0
NB.   out =. y TagLine '<letStatement>'

NB.   out =. out , (y + 2) Consume ''  NB. let
NB.   out =. out , (y + 2) Consume ''  NB. varName

NB.   if. TokenIs '[' do.
NB.     out =. out , (y + 2) Consume ''  NB. [
NB.     out =. out , CompileExpression (y + 2)
NB.     out =. out , (y + 2) Consume ''  NB. ]
NB.   end.

NB.   out =. out , (y + 2) Consume ''  NB. =
NB.   out =. out , CompileExpression (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. ;

NB.   out =. out , y TagLine '</letStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. ifStatement
NB. NB. ============================================================

NB. CompileIf =: 3 : 0
NB.   out =. y TagLine '<ifStatement>'

NB.   out =. out , (y + 2) Consume ''  NB. if
NB.   out =. out , (y + 2) Consume ''  NB. (
NB.   out =. out , CompileExpression (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. )
NB.   out =. out , (y + 2) Consume ''  NB. {
NB.   out =. out , CompileStatements (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. }

NB.   if. TokenIs 'else' do.
NB.     out =. out , (y + 2) Consume ''  NB. else
NB.     out =. out , (y + 2) Consume ''  NB. {
NB.     out =. out , CompileStatements (y + 2)
NB.     out =. out , (y + 2) Consume ''  NB. }
NB.   end.

NB.   out =. out , y TagLine '</ifStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. whileStatement
NB. NB. ============================================================

NB. CompileWhile =: 3 : 0
NB.   out =. y TagLine '<whileStatement>'

NB.   out =. out , (y + 2) Consume ''  NB. while
NB.   out =. out , (y + 2) Consume ''  NB. (
NB.   out =. out , CompileExpression (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. )
NB.   out =. out , (y + 2) Consume ''  NB. {
NB.   out =. out , CompileStatements (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. }

NB.   out =. out , y TagLine '</whileStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. doStatement
NB. NB. ============================================================

NB. CompileDo =: 3 : 0
NB.   out =. y TagLine '<doStatement>'

NB.   out =. out , (y + 2) Consume ''  NB. do
NB.   out =. out , CompileSubroutineCall (y + 2)
NB.   out =. out , (y + 2) Consume ''  NB. ;

NB.   out =. out , y TagLine '</doStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. returnStatement
NB. NB. ============================================================

NB. CompileReturn =: 3 : 0
NB.   out =. y TagLine '<returnStatement>'

NB.   out =. out , (y + 2) Consume ''  NB. return

NB.   if. -. TokenIs ';' do.
NB.     out =. out , CompileExpression (y + 2)
NB.   end.

NB.   out =. out , (y + 2) Consume ''  NB. ;

NB.   out =. out , y TagLine '</returnStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. expression
NB. NB. ============================================================

NB. CompileExpression =: 3 : 0
NB.   out =. y TagLine '<expression>'

NB.   out =. out , CompileTerm (y + 2)

NB.   while. IsOp '' do.
NB.     out =. out , (y + 2) Consume ''  NB. operator
NB.     out =. out , CompileTerm (y + 2)
NB.   end.

NB.   out =. out , y TagLine '</expression>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. term
NB. NB. ============================================================

NB. CompileTerm =: 3 : 0
NB.   out =. y TagLine '<term>'

NB.   if. TokenIs '(' do.

NB.     out =. out , (y + 2) Consume ''  NB. (
NB.     out =. out , CompileExpression (y + 2)
NB.     out =. out , (y + 2) Consume ''  NB. )

NB.   elseif. (TokenIs '-') +. (TokenIs '~') do.

NB.     out =. out , (y + 2) Consume ''  NB. unary op
NB.     out =. out , CompileTerm (y + 2)

NB.   elseif. IsIdentifier '' do.

NB.     if. NextTokenIs '[' do.

NB.       out =. out , (y + 2) Consume ''  NB. varName
NB.       out =. out , (y + 2) Consume ''  NB. [
NB.       out =. out , CompileExpression (y + 2)
NB.       out =. out , (y + 2) Consume ''  NB. ]

NB.     elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.

NB.       out =. out , CompileSubroutineCall (y + 2)

NB.     else.

NB.       out =. out , (y + 2) Consume ''

NB.     end.

NB.   else.

NB.     out =. out , (y + 2) Consume ''

NB.   end.

NB.   out =. out , y TagLine '</term>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. expressionList
NB. NB. ============================================================

NB. CompileExpressionList =: 3 : 0
NB.   out =. y TagLine '<expressionList>'

NB.   if. -. TokenIs ')' do.
NB.     out =. out , CompileExpression (y + 2)

NB.     while. TokenIs ',' do.
NB.       out =. out , (y + 2) Consume ''  NB. ,
NB.       out =. out , CompileExpression (y + 2)
NB.     end.
NB.   end.

NB.   out =. out , y TagLine '</expressionList>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. subroutineCall
NB. NB. ============================================================

NB. CompileSubroutineCall =: 3 : 0
NB.   out =. ''

NB.   out =. out , y Consume ''

NB.   if. TokenIs '.' do.
NB.     out =. out , y Consume ''
NB.     out =. out , y Consume ''
NB.   end.

NB.   out =. out , y Consume ''  NB. (
NB.   out =. out , CompileExpressionList y
NB.   out =. out , y Consume ''  NB. )

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. file handling
NB. NB. ============================================================

NB. ParseFile =: 3 : 0
NB.   inputFile =. y

NB.   NB. Part A: create XxxT.xml first
NB.   AnalyzeFile inputFile

NB.   base =. ((# inputFile) - 5) {. inputFile

NB.   tokenFile =. base , 'T.xml'
NB.   outputFile =. base , '.xml'

NB.   tokenLines =. ReadParserTokens tokenFile

NB.   finalXml =. CompileClass tokenLines

NB.   finalXml WriteFile outputFile

NB.   smoutput 'Written: ' , outputFile
NB. )

NB. NB. ============================================================
NB. NB. Main
NB. NB. ============================================================

NB. Main =: 3 : 0
NB.   path =. y

NB.   if. '.jack' -: _5 {. path do.
NB.     ParseFile path
NB.     return.
NB.   end.

NB.   files =. GetJackFiles path

NB.   if. 0 = # files do.
NB.     smoutput 'No .jack files found in: ' , path
NB.     return.
NB.   end.

NB.   for_f. files do.
NB.     ParseFile > f
NB.   end.

NB.   0
NB. )

NB. smoutput 'parser.ijs loaded.'