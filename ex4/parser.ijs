NB. ============================================================
NB. parser.ijs - Jack Parser (Project 10, Part B)
NB. ============================================================

NB. Load tokenizer utilities from part A
load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. ------- Global variables -------

tokens  =: 0 # a:
current =: 0
output  =: ''
LF      =: a. {~ 10

NB. ------- Load token xml -------

NB. Load tokens from XxxT.xml
LoadTokens =: 3 : 0
  lines =. <;._2 ReadFile y
  tokens =: }. }: lines
  current =: 0
  output =: ''
)

NB. ------- Token access -------

NB. Return current token
Peek =: 3 : 0
  > current { tokens
)

NB. Return token by offset
PeekN =: 3 : 0
  > (current + y) { tokens
)

NB. Return current token and move forward
Advance =: 3 : 0
  tok =. > current { tokens
  current =: current + 1
  tok
)

NB. ------- Output -------

NB. Add line to output
Write =: 3 : 0
  output =: output , y , LF
)

NB. Save xml output
SaveOutput =: 3 : 0
  output WriteFile y
)

NB. ------- Token checks -------

NB. Check current keyword
IsKeyword =: 3 : 0
  ('<keyword> ' , y , ' </keyword>') -: Peek ''
)

NB. Check current symbol
IsSymbol =: 3 : 0
  ('<symbol> ' , y , ' </symbol>') -: Peek ''
)

NB. Check token prefix
StartsWith =: 4 : 0
  x -: (#x) {. y
)

NB. Check current identifier
IsIdentifier =: 3 : 0
  '<identifier>' StartsWith Peek ''
)

NB. Check current integer
IsInteger =: 3 : 0
  '<integerConstant>' StartsWith Peek ''
)

NB. Check current string
IsString =: 3 : 0
  '<stringConstant>' StartsWith Peek ''
)

NB. Check expression operator
IsOp =: 3 : 0
  (IsSymbol '+') +. (IsSymbol '-') +. (IsSymbol '*') +. (IsSymbol '/') +. (IsSymbol '&amp;') +. (IsSymbol '|') +. (IsSymbol '&lt;') +. (IsSymbol '&gt;') +. (IsSymbol '=')
)

NB. ------- Expressions -------

NB. Compile expression
CompileExpression =: 3 : 0
  Write '<expression>'
  CompileTerm ''

  while. IsOp '' do.
    Write Advance ''
    CompileTerm ''
  end.

  Write '</expression>'
)

NB. Compile term
CompileTerm =: 3 : 0
  Write '<term>'

  if. (IsInteger '') +. (IsString '') +. (IsKeyword 'true') +. (IsKeyword 'false') +. (IsKeyword 'null') +. (IsKeyword 'this') do.
    Write Advance ''

  elseif. IsSymbol '(' do.
    Write Advance ''
    CompileExpression ''
    Write Advance ''

  elseif. (IsSymbol '-') +. (IsSymbol '~') do.
    Write Advance ''
    CompileTerm ''

  elseif. IsIdentifier '' do.
    if. '<symbol> [ </symbol>' -: PeekN 1 do.
      Write Advance ''
      Write Advance ''
      CompileExpression ''
      Write Advance ''

    elseif. (('<symbol> ( </symbol>' -: PeekN 1) +. ('<symbol> . </symbol>' -: PeekN 1)) do.
      CompileSubroutineCall ''

    else.
      Write Advance ''
    end.
  end.

  Write '</term>'
)

NB. Compile expression list
CompileExpressionList =: 3 : 0
  Write '<expressionList>'

  if. -. IsSymbol ')' do.
    CompileExpression ''

    while. IsSymbol ',' do.
      Write Advance ''
      CompileExpression ''
    end.
  end.

  Write '</expressionList>'
)

NB. Compile subroutine call without extra tag
CompileSubroutineCall =: 3 : 0
  Write Advance ''

  if. IsSymbol '.' do.
    Write Advance ''
    Write Advance ''
  end.

  Write Advance ''
  CompileExpressionList ''
  Write Advance ''
)

NB. ------- Statements -------

NB. Compile statements block
CompileStatements =: 3 : 0
  Write '<statements>'

  while. ((IsKeyword 'let') +. (IsKeyword 'if') +. (IsKeyword 'while') +. (IsKeyword 'do') +. (IsKeyword 'return')) do.
    if. IsKeyword 'let' do.
      CompileLet ''
    elseif. IsKeyword 'if' do.
      CompileIf ''
    elseif. IsKeyword 'while' do.
      CompileWhile ''
    elseif. IsKeyword 'do' do.
      CompileDo ''
    elseif. IsKeyword 'return' do.
      CompileReturn ''
    end.
  end.

  Write '</statements>'
)

NB. Compile let statement
CompileLet =: 3 : 0
  Write '<letStatement>'

  Write Advance ''
  Write Advance ''

  if. IsSymbol '[' do.
    Write Advance ''
    CompileExpression ''
    Write Advance ''
  end.

  Write Advance ''
  CompileExpression ''
  Write Advance ''

  Write '</letStatement>'
)

NB. Compile if statement
CompileIf =: 3 : 0
  Write '<ifStatement>'

  Write Advance ''
  Write Advance ''
  CompileExpression ''
  Write Advance ''
  Write Advance ''
  CompileStatements ''
  Write Advance ''

  if. IsKeyword 'else' do.
    Write Advance ''
    Write Advance ''
    CompileStatements ''
    Write Advance ''
  end.

  Write '</ifStatement>'
)

NB. Compile while statement
CompileWhile =: 3 : 0
  Write '<whileStatement>'

  Write Advance ''
  Write Advance ''
  CompileExpression ''
  Write Advance ''
  Write Advance ''
  CompileStatements ''
  Write Advance ''

  Write '</whileStatement>'
)

NB. Compile do statement
CompileDo =: 3 : 0
  Write '<doStatement>'

  Write Advance ''
  CompileSubroutineCall ''
  Write Advance ''

  Write '</doStatement>'
)

NB. Compile return statement
CompileReturn =: 3 : 0
  Write '<returnStatement>'

  Write Advance ''

  if. -. IsSymbol ';' do.
    CompileExpression ''
  end.

  Write Advance ''

  Write '</returnStatement>'
)

NB. ------- Declarations -------

NB. Compile var declaration
CompileVarDec =: 3 : 0
  Write '<varDec>'

  while. -. IsSymbol ';' do.
    Write Advance ''
  end.

  Write Advance ''

  Write '</varDec>'
)

NB. Compile class variable declaration
CompileClassVarDec =: 3 : 0
  Write '<classVarDec>'

  while. -. IsSymbol ';' do.
    Write Advance ''
  end.

  Write Advance ''

  Write '</classVarDec>'
)

NB. Compile parameter list
CompileParameterList =: 3 : 0
  Write '<parameterList>'

  while. -. IsSymbol ')' do.
    Write Advance ''
  end.

  Write '</parameterList>'
)

NB. Compile subroutine body
CompileSubroutineBody =: 3 : 0
  Write '<subroutineBody>'

  Write Advance ''

  while. IsKeyword 'var' do.
    CompileVarDec ''
  end.

  CompileStatements ''

  Write Advance ''

  Write '</subroutineBody>'
)

NB. Compile subroutine declaration
CompileSubroutine =: 3 : 0
  Write '<subroutineDec>'

  Write Advance ''
  Write Advance ''
  Write Advance ''
  Write Advance ''

  CompileParameterList ''

  Write Advance ''

  CompileSubroutineBody ''

  Write '</subroutineDec>'
)

NB. ------- Class -------

NB. Compile class structure
CompileClass =: 3 : 0
  Write '<class>'

  Write Advance ''
  Write Advance ''
  Write Advance ''

  while. ((IsKeyword 'static') +. (IsKeyword 'field')) do.
    CompileClassVarDec ''
  end.

  while. ((IsKeyword 'constructor') +. (IsKeyword 'function') +. (IsKeyword 'method')) do.
    CompileSubroutine ''
  end.

  Write Advance ''

  Write '</class>'
)

NB. Startup message
smoutput 'parser.ijs loaded.'