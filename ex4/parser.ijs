NB. ============================================================
NB. parser.ijs - Jack Parser / Compilation Engine
NB. ============================================================

load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

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

  out =. out , 2 Consume ''  NB. class
  out =. out , 2 Consume ''  NB. className
  out =. out , 2 Consume ''  NB. {

  while. (TokenIs 'static') +. (TokenIs 'field') do.
    out =. out , CompileClassVarDec ''
  end.

  while. (TokenIs 'constructor') +. (TokenIs 'function') +. (TokenIs 'method') do.
    out =. out , CompileSubroutine ''
  end.

  out =. out , 2 Consume ''  NB. }
  out =. out , '</class>' , LF

  out
)

NB. ============================================================
NB. classVarDec
NB. ============================================================

CompileClassVarDec =: 3 : 0
  out =. 2 TagLine '<classVarDec>'

  out =. out , 4 Consume ''  NB. static / field
  out =. out , 4 Consume ''  NB. type
  out =. out , 4 Consume ''  NB. varName

  while. TokenIs ',' do.
    out =. out , 4 Consume ''  NB. ,
    out =. out , 4 Consume ''  NB. varName
  end.

  out =. out , 4 Consume ''  NB. ;
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