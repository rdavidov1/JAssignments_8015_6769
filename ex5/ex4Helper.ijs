NB. ============================================================
NB. ex4.ijs - Jack Tokenizer (nand2tetris Project 10, Stage 1)
NB. ============================================================
NB. Usage (from jconsole):
NB.   load 'ex4.ijs'
NB.   Main '/path/to/SomeFile.jack'
NB.   Main '/path/to/SomeFolder'
NB. ============================================================

NB. load 'C:/Users/Home/Documents/temp/ex5/ex5.ijs'

NB. ------- Global constants -------

NB. Jack keywords as boxed list
KEYWORDS =: ;: 'class constructor function method field static var int char boolean void true false null this let do if else while return'

NB. Jack symbol characters (each is a single-token symbol)
SYMBOLS =: '{}()[].,;+-*/&|<>=~'

NB. Whitespace: space, tab(9), LF(10), CR(13)
WS =: ' ' , (a.{~9) , (a.{~10) , (a.{~13)

NB. All letter characters a-z and A-Z
LETTERS =: (a.{~(a.i.'a')+i.26) , (a.{~(a.i.'A')+i.26)

NB. Digit characters
DIGITS =: '0123456789'

NB. ------- File I/O -------

NB. ReadFile 'path' -- reads file as flat character vector
ReadFile =: 1!:1 @ boxopen

NB. 'content' WriteFile 'path' -- writes string to file
WriteFile =: 4 : 0
  x 1!:2 < y
)
NB. ------- Character predicates -------

NB. IsDigit c -- 1 iff c is a digit character
IsDigit =: 3 : 0
  y e. DIGITS
)

NB. IsLetter c -- 1 iff c is a-z or A-Z
IsLetter =: 3 : 0
  y e. LETTERS
)
NB. IsIdentifierChar c -- 1 iff c can appear inside an identifier

IsIdentifierChar =: 3 : 0
  (IsLetter y) +. (IsDigit y) +. y = '_'
)

NB. IsSpace c -- 1 iff c is whitespace
IsSpace =: 3 : 0
  y e. WS
)
NB. ------- XML escaping -------

NB. EscapeXml 'str' -- replaces <, >, &, " with XML entities
EscapeXml =: 3 : 0
  r =. ''
  for_c. y do.
    select. c
      case. '<' do. r =. r , '&lt;'
      case. '>' do. r =. r , '&gt;'
      case. '"' do. r =. r , '&quot;'
      case. '&' do. r =. r , '&amp;'
      case.      do. r =. r , c
    end.
  end.
  r
)

NB. ------- Tokenizer -------

NB. Tokenize 'src' -- given Jack source text, return boxed list of (type;value) pairs
Tokenize =: 3 : 0
  src   =. y
  n     =. # src
  i     =. 0
  tlist =. 0 # a:   NB. empty boxed list to accumulate tokens
  LF    =. a. {~ 10

  while. i < n do.
    c =. i { src

    NB. ---- Skip whitespace ----
    if. IsSpace c do.
      i =. i + 1
      continue.
    end.

    NB. ---- Comments ----
    if. (c = '/') *. (i + 1 < n) do.
      nc =. (i + 1) { src

      NB. Line comment: // to end of line
      if. nc = '/' do.
        i =. i + 2
        while. (i < n) *. (LF ~: i { src) do. i =. i + 1 end.
        continue.
      end.

      NB. Block comment: /* ... */ (includes doc comments /** ... */)
      if. nc = '*' do.
        i =. i + 2
        closed =. 0
        while. (i + 1 < n) *. -. closed do.
          if. ('*' = i { src) *. ('/' = (i + 1) { src) do.
            i      =. i + 2
            closed =. 1
          else.
            i =. i + 1
          end.
        end.
        if. -. closed do. i =. n end.  NB. safety: skip to end if unclosed
        continue.
      end.
    end.

    NB. ---- String constant: "..." ----
    if. c = '"' do.
      i =. i + 1           NB. skip opening double-quote
      j =. i
      while. (j < n) *. ('"' ~: j { src) do. j =. j + 1 end.
      val   =. (j - i) {. i }. src   NB. content between quotes
      tlist =. tlist , < ('stringConstant' ; val)
      i     =. j + 1                  NB. skip closing double-quote
      continue.
    end.

    NB. ---- Integer constant: digit sequence ----
    if. IsDigit c do.
      j =. i + 1
      while. (j < n) *. IsDigit (j { src) do. j =. j + 1 end.
      val   =. (j - i) {. i }. src
      tlist =. tlist , < ('integerConstant' ; val)
      i     =. j
      continue.
    end.

    NB. ---- Symbol: single-character token ----
    if. c e. SYMBOLS do.
      tlist =. tlist , < ('symbol' ; (1 {. i }. src))
      i     =. i + 1
      continue.
    end.

    NB. ---- Keyword or Identifier: starts with letter or underscore ----
    if. (IsLetter c) +. (c = '_') do.
      j =. i + 1
      while. (j < n) *. IsIdentifierChar (j { src) do. j =. j + 1 end.
      val =. (j - i) {. i }. src
      NB. Check if the word is a reserved keyword
      if. (<val) e. KEYWORDS do.
        tlist =. tlist , < ('keyword' ; val)
      else.
        tlist =. tlist , < ('identifier' ; val)
      end.
      i =. j
      continue.
    end.

    NB. ---- Unknown/unexpected character: skip silently ----
    i =. i + 1
  end.

  tlist
)

NB. ------- Token -> XML line -------

NB. TokenToXml tok -- tok is a box holding (type ; value)
NB. Produces:  <type> escaped_value </type>
TokenToXml =: 3 : 0
  typ     =. > 0 { y
  val     =. > 1 { y
  escaped =. EscapeXml val
  '<' , typ , '> ' , escaped , ' </' , typ , '>'
)

NB. ------- Analyze a single .jack file -------

NB. AnalyzeFile 'path/to/Xxx.jack' -- writes 'path/to/XxxT.xml'
AnalyzeFile =: 3 : 0
  path =. y
  LF   =. a. {~ 10

  NB. Build output path: strip '.jack', append 'T.xml'
  NB. Find position of last '.' in path
  dotpos =. <: # path    NB. default fallback
  k      =. <: # path
  while. k >: 0 do.
    if. '.' = k { path do.
      dotpos =. k
      k      =. _1        NB. signal loop exit
    else.
      k =. k - 1
    end.
  end.
  base    =. dotpos {. path       NB. path without extension
  outpath =. base , 'T.xml'

  NB. Read source, tokenize, format XML
  src   =. ReadFile path
  tlist =. Tokenize src

  xml =. '<tokens>' , LF
  for_tok. tlist do.
    xml =. xml , (TokenToXml > tok) , LF
  end.
  xml =. xml , '</tokens>' , LF

  NB. Write output
  xml WriteFile outpath
  smoutput 'Written: ' , outpath
)

NB. ------- Directory listing: get .jack files from a folder -------
NB.for Hagit
NB. GetJackFiles 'folder' -- returns boxed list of full paths to .jack files
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

NB.for Ravid
GetJackFiles =: 3 : 0
  dir =. y
  if. '/' = {: dir do.
    dir =. }: dir
  end.
  files =. 1!:0 < dir , '/*.jack'
  if. 0 = # files do.
    0 # a:
  else.
    names =. {."1 files
    (< dir , '/') ,&.> names
  end.
)

NB. ------- Main -------

NB. Main 'path' -- path is either Xxx.jack or a folder
Main =: 3 : 0
  path =. y

  NB. Single-file mode: path ends with '.jack'
  if. '.jack' -: (_5) {. path do.
    AnalyzeFile path
    return.
  end.

  NB. Folder mode: process every .jack file in the folder
  jackfiles =. GetJackFiles path
  if. 0 = # jackfiles do.
    smoutput 'No .jack files found in: ' , path
    return.
  end.
  for_f. jackfiles do.
    AnalyzeFile > f
  end.
)

NB. Startup message
smoutput 'ex4.ijs loaded. Jack Tokenizer ready.'
smoutput '  Single file:  Main ''/path/to/Xxx.jack'''
smoutput '  Folder:       Main ''/path/to/Folder'''

NB. ============================================================
NB. VM Writer
NB. ============================================================

vmOut =: ''

WriteVM =: 3 : 0
  vmOut =: vmOut , y , LF
)

WritePush =: 4 : 0
  WriteVM 'push ' , x , ' ' , ": y
)

WritePop =: 4 : 0
  WriteVM 'pop ' , x , ' ' , ": y
)

WriteArithmetic =: 3 : 0
  WriteVM y
)

WriteCall =: 4 : 0
  WriteVM 'call ' , x , ' ' , ": y
)

WriteFunction =: 4 : 0
  WriteVM 'function ' , x , ' ' , ": y
)

WriteReturn =: 3 : 0
  WriteVM 'return'
)

NB. ============================================================
NB. Part B parser.ijs - Jack Parser / Compilation Engine
NB. ============================================================


LF =: a. {~ 10

NB. ------- Parser state -------

tokens_list =: 0 # a:
pointer =: 0

NB. Create spaces for XML indentation
Spaces =: 3 : 0
  y # ' '
)

NB. Create one XML line with indentation
TagLine =: 4 : 0
  (Spaces x) , y , LF
)

NB. Get token by index
GetToken =: 3 : 0
  if. y < # tokens_list do.
    > y { tokens_list
  else.
    ''
  end.
)

NB. Get current token
CurrentToken =: 3 : 0
  GetToken pointer
)

NB. Move to next token
MoveNext =: 3 : 0
  pointer =: pointer + 1
)

NB. Write current token and advance
Consume =: 4 : 0
  tok =. CurrentToken ''
  MoveNext ''
  (Spaces x) , tok , LF
)

NB. Check if current token contains a specific value
TokenIs =: 3 : 0
  1 e. (' ' , y , ' ') E. CurrentToken ''
)

NB. Check next token value
NextTokenIs =: 3 : 0
  1 e. (' ' , y , ' ') E. GetToken pointer + 1
)

NB. Check if current token contains text
CurrentHas =: 3 : 0
  1 e. y E. CurrentToken ''
)

NB. Check identifier token
IsIdentifier =: 3 : 0
  CurrentHas '<identifier>'
)

NB. Check expression operator
IsOp =: 3 : 0
  (TokenIs '+') +. (TokenIs '-') +. (TokenIs '*') +. (TokenIs '/') +. (TokenIs '&amp;') +. (TokenIs '|') +. (TokenIs '&lt;') +. (TokenIs '&gt;') +. (TokenIs '=')
)

NB. ------- Load tokens from XxxT.xml -------

ReadParserTokens =: 3 : 0
  lines =. <;._2 ReadFile y
  lines =. dltb each lines
  lines =. lines -. <''
  lines =. lines -. <'<tokens>'
  lines =. lines -. <'</tokens>'
  lines
)

NB. ============================================================
NB. class
NB. ============================================================
CompileClass =: 3 : 0

  tokens_list =: y
  pointer =: 0

  out =. '<class>' , LF

    NB. class
    out =. out , 2 Consume ''
  className =: GetTokenValue CurrentToken ''

  out =. out , 2 Consume ''      NB. className

  out =. out , 2 Consume ''      NB. {

  while. (TokenIs 'static') +. (TokenIs 'field') do.

      out =. out , CompileClassVarDec ''

  end.

    while. (TokenIs 'constructor') +. (TokenIs 'function') +. (TokenIs 'method') do.

        out =. out , CompileSubroutine ''

    end.


  out =. out , 2 Consume ''      NB. }

  out =. out , '</class>' , LF

  out

)


NB. ============================================================
NB. classVarDec
NB. ============================================================
CompileClassVarDec =: 3 : 0

  out =. 2 TagLine '<classVarDec>'

  kind =. GetTokenValue CurrentToken ''

  out =. out , 4 Consume ''

  type =. GetTokenValue CurrentToken ''

  out =. out , 4 Consume ''

  name =. GetTokenValue CurrentToken ''

  name Define (type ; kind)

  out =. out , 4 Consume ''

  while. TokenIs ',' do.

    out =. out , 4 Consume ''

    name =. GetTokenValue CurrentToken ''

    name Define (type ; kind)

    out =. out , 4 Consume ''

  end.

  out =. out , 4 Consume ''

  out =. out , 2 TagLine '</classVarDec>'

  out

)

NB. ============================================================
NB. subroutineDec
NB. ============================================================

CompileSubroutine =: 3 : 0
  out =. 2 TagLine '<subroutineDec>'

  out =. out , 4 Consume ''  NB. constructor / function / method
  out =. out , 4 Consume ''  NB. return type
  out =. out , 4 Consume ''  NB. subroutineName
  out =. out , 4 Consume ''  NB. (

  out =. out , CompileParameterList 4

  out =. out , 4 Consume ''  NB. )

  out =. out , CompileSubroutineBody 4

  out =. out , 2 TagLine '</subroutineDec>'

  out
)

NB. ============================================================
NB. parameterList
NB. ============================================================

CompileParameterList =: 3 : 0
  out =. y TagLine '<parameterList>'

  if. -. TokenIs ')' do.
    out =. out , (y + 2) Consume ''  NB. type
    out =. out , (y + 2) Consume ''  NB. varName

    while. TokenIs ',' do.
      out =. out , (y + 2) Consume ''  NB. ,
      out =. out , (y + 2) Consume ''  NB. type
      out =. out , (y + 2) Consume ''  NB. varName
    end.
  end.

  out =. out , y TagLine '</parameterList>'

  out
)

NB. ============================================================
NB. subroutineBody
NB. ============================================================

CompileSubroutineBody =: 3 : 0
  out =. y TagLine '<subroutineBody>'

  out =. out , (y + 2) Consume ''  NB. {

  while. TokenIs 'var' do.
    out =. out , CompileVarDec (y + 2)
  end.

  out =. out , CompileStatements (y + 2)

  out =. out , (y + 2) Consume ''  NB. }

  out =. out , y TagLine '</subroutineBody>'

  out
)

NB. ============================================================
NB. varDec
NB. ============================================================

CompileVarDec =: 3 : 0
  out =. y TagLine '<varDec>'

  out =. out , (y + 2) Consume ''  NB. var
  out =. out , (y + 2) Consume ''  NB. type
  out =. out , (y + 2) Consume ''  NB. varName

  while. TokenIs ',' do.
    out =. out , (y + 2) Consume ''  NB. ,
    out =. out , (y + 2) Consume ''  NB. varName
  end.

  out =. out , (y + 2) Consume ''  NB. ;
  out =. out , y TagLine '</varDec>'

  out
)

NB. ============================================================
NB. statements
NB. ============================================================

CompileStatements =: 3 : 0
  out =. y TagLine '<statements>'

  while. (TokenIs 'let') +. (TokenIs 'if') +. (TokenIs 'while') +. (TokenIs 'do') +. (TokenIs 'return') do.

    if. TokenIs 'let' do.
      out =. out , CompileLet (y + 2)

    elseif. TokenIs 'if' do.
      out =. out , CompileIf (y + 2)

    elseif. TokenIs 'while' do.
      out =. out , CompileWhile (y + 2)

    elseif. TokenIs 'do' do.
      out =. out , CompileDo (y + 2)

    elseif. TokenIs 'return' do.
      out =. out , CompileReturn (y + 2)

    end.
  end.

  out =. out , y TagLine '</statements>'

  out
)

NB. ============================================================
NB. letStatement
NB. ============================================================

CompileLet =: 3 : 0
  out =. y TagLine '<letStatement>'

  out =. out , (y + 2) Consume ''  NB. let
  out =. out , (y + 2) Consume ''  NB. varName

  if. TokenIs '[' do.
    out =. out , (y + 2) Consume ''  NB. [
    out =. out , CompileExpression (y + 2)
    out =. out , (y + 2) Consume ''  NB. ]
  end.

  out =. out , (y + 2) Consume ''  NB. =
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''  NB. ;

  out =. out , y TagLine '</letStatement>'

  out
)

NB. ============================================================
NB. ifStatement
NB. ============================================================

CompileIf =: 3 : 0
  out =. y TagLine '<ifStatement>'

  out =. out , (y + 2) Consume ''  NB. if
  out =. out , (y + 2) Consume ''  NB. (
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''  NB. )
  out =. out , (y + 2) Consume ''  NB. {
  out =. out , CompileStatements (y + 2)
  out =. out , (y + 2) Consume ''  NB. }

  if. TokenIs 'else' do.
    out =. out , (y + 2) Consume ''  NB. else
    out =. out , (y + 2) Consume ''  NB. {
    out =. out , CompileStatements (y + 2)
    out =. out , (y + 2) Consume ''  NB. }
  end.

  out =. out , y TagLine '</ifStatement>'

  out
)

NB. ============================================================
NB. whileStatement
NB. ============================================================

CompileWhile =: 3 : 0
  out =. y TagLine '<whileStatement>'

  out =. out , (y + 2) Consume ''  NB. while
  out =. out , (y + 2) Consume ''  NB. (
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''  NB. )
  out =. out , (y + 2) Consume ''  NB. {
  out =. out , CompileStatements (y + 2)
  out =. out , (y + 2) Consume ''  NB. }

  out =. out , y TagLine '</whileStatement>'

  out
)

NB. ============================================================
NB. doStatement
NB. ============================================================

CompileDo =: 3 : 0
  out =. y TagLine '<doStatement>'

  out =. out , (y + 2) Consume ''  NB. do
  out =. out , CompileSubroutineCall (y + 2)
  out =. out , (y + 2) Consume ''  NB. ;

  out =. out , y TagLine '</doStatement>'

  out
)

NB. ============================================================
NB. returnStatement
NB. ============================================================

CompileReturn =: 3 : 0
  out =. y TagLine '<returnStatement>'

  out =. out , (y + 2) Consume ''  NB. return

  if. -. TokenIs ';' do.
    out =. out , CompileExpression (y + 2)
  end.

  out =. out , (y + 2) Consume ''  NB. ;

  out =. out , y TagLine '</returnStatement>'

  out
)

NB. ============================================================
NB. expression
NB. ============================================================

CompileExpression =: 3 : 0
  out =. y TagLine '<expression>'

  out =. out , CompileTerm (y + 2)

  while. IsOp '' do.
    out =. out , (y + 2) Consume ''  NB. operator
    out =. out , CompileTerm (y + 2)
  end.

  out =. out , y TagLine '</expression>'

  out
)

NB. ============================================================
NB. Extract token value from XML token
NB. Example:
NB. <integerConstant> 7 </integerConstant>
NB. -> 7
NB. ============================================================

GetTokenValue =: 3 : 0

  tok =. y

  gt =. tok i. '>'

  rest =. (gt+1) }. tok

  lt =. rest i. '<'

  dltb lt {. rest

)

NB. ============================================================
NB. term
NB. ============================================================

CompileTerm =: 3 : 0
  out =. y TagLine '<term>'

  if. TokenIs '(' do.

    out =. out , (y + 2) Consume ''  NB. (
    out =. out , CompileExpression (y + 2)
    out =. out , (y + 2) Consume ''  NB. )

  elseif. (TokenIs '-') +. (TokenIs '~') do.

    out =. out , (y + 2) Consume ''  NB. unary op
    out =. out , CompileTerm (y + 2)

  elseif. IsIdentifier '' do.

    if. NextTokenIs '[' do.

      out =. out , (y + 2) Consume ''  NB. varName
      out =. out , (y + 2) Consume ''  NB. [
      out =. out , CompileExpression (y + 2)
      out =. out , (y + 2) Consume ''  NB. ]

    elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.

      out =. out , CompileSubroutineCall (y + 2)

    else.

      out =. out , (y + 2) Consume ''

    end.

  else.

    out =. out , (y + 2) Consume ''

  end.

  out =. out , y TagLine '</term>'

  out
)

NB. ============================================================
NB. expressionList
NB. ============================================================

CompileExpressionList =: 3 : 0
  out =. y TagLine '<expressionList>'

  if. -. TokenIs ')' do.
    out =. out , CompileExpression (y + 2)

    while. TokenIs ',' do.
      out =. out , (y + 2) Consume ''  NB. ,
      out =. out , CompileExpression (y + 2)
    end.
  end.

  out =. out , y TagLine '</expressionList>'

  out
)

NB. ============================================================
NB. subroutineCall
NB. ============================================================

CompileSubroutineCall =: 3 : 0
  out =. ''

  out =. out , y Consume ''

  if. TokenIs '.' do.
    out =. out , y Consume ''
    out =. out , y Consume ''
  end.

  out =. out , y Consume ''  NB. (
  out =. out , CompileExpressionList y
  out =. out , y Consume ''  NB. )

  out
)

NB. ============================================================
NB. file handling
NB. ============================================================

ParseFile =: 3 : 0
  inputFile =. y

  NB. Part A: create XxxT.xml first
  AnalyzeFile inputFile

  base =. ((# inputFile) - 5) {. inputFile

  tokenFile =. base , 'T.xml'
  outputFile =. base , '.xml'
  tokenLines =. ReadParserTokens tokenFile

  finalXml =. CompileClass tokenLines

    finalXml WriteFile outputFile

  smoutput 'Written: ' , outputFile
)

NB. ============================================================
NB. Main
NB. ============================================================

Main =: 3 : 0
  path =. y

  if. '.jack' -: _5 {. path do.
    ParseFile path
    return.
  end.

  files =. GetJackFiles path

  if. 0 = # files do.
    smoutput 'No .jack files found in: ' , path
    return.
  end.

  for_f. files do.
    ParseFile > f
  end.

  0
)

smoutput 'parser.ijs loaded.'