NB. ============================================================
NB. ex4.ijs - Jack Tokenizer (nand2tetris Project 10, Stage 1)
NB. ============================================================
NB. Usage (from jconsole):
NB.   load 'ex4.ijs'
NB.   Main '/path/to/SomeFile.jack'
NB.   Main '/path/to/SomeFolder'
NB. ============================================================


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

NB. GetJackFiles 'folder' -- returns boxed list of full paths to .jack files
GetJackFiles =: 3 : 0
  folder =. y
  LF =. a. {~ 10

  if. -. ({: folder) e. '/\' do.
    folder =. folder , '/'
  end.

  raw =. shell 'ls "' , folder , '"*.jack 2>/dev/null'

  if. 0 = # raw do.
    0 # a:
  else.
    files =. <;._2 raw , LF
    files =. files #~ 0 < #&> files
    files
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


