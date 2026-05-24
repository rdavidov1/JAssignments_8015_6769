NB. ============================================================
NB. parser.ijs - Jack Parser (Project 10, Part B)
NB. ============================================================

NB. Load tokenizer utilities from part A
load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. ------- Global variables -------

tokens  =: 0 # a:
current =: 0
output  =: ''
indent  =: 0
LF      =: a. {~ 10

NB. ------- Load token xml -------

NB. Load tokens from XxxT.xml
LoadTokens =: 3 : 0
  lines =. <;._2 ReadFile y
  tokens =: }. }: lines
  current =: 0
  output =: ''
  indent =: 0
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

NB. Add line with indentation
Write =: 3 : 0
  spaces =. (2 * indent) # ' '
  output =: output , spaces , y , LF
)

NB. Open non-terminal tag
OpenTag =: 3 : 0
  Write '<' , y , '>'
  indent =: indent + 1
)

NB. Close non-terminal tag
CloseTag =: 3 : 0
  indent =: indent - 1
  Write '</' , y , '>'
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
  OpenTag 'expression'
  CompileTerm ''

  while. IsOp '' do.
    Write Advance ''
    CompileTerm ''
  end.

  CloseTag 'expression'
)

NB. Compile term
CompileTerm =: 3 : 0
  OpenTag 'term'

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

  CloseTag 'term'
)

NB. Compile expression list
CompileExpressionList =: 3 : 0
  OpenTag 'expressionList'

  if. -. IsSymbol ')' do.
    CompileExpression ''

    while. IsSymbol ',' do.
      Write Advance ''
      CompileExpression ''
    end.
  end.

  CloseTag 'expressionList'
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
  OpenTag 'statements'

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

  CloseTag 'statements'
)

NB. Compile let statement
CompileLet =: 3 : 0
  OpenTag 'letStatement'

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

  CloseTag 'letStatement'
)

NB. Compile if statement
CompileIf =: 3 : 0
  OpenTag 'ifStatement'

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

  CloseTag 'ifStatement'
)

NB. Compile while statement
CompileWhile =: 3 : 0
  OpenTag 'whileStatement'

  Write Advance ''
  Write Advance ''
  CompileExpression ''
  Write Advance ''
  Write Advance ''
  CompileStatements ''
  Write Advance ''

  CloseTag 'whileStatement'
)

NB. Compile do statement
CompileDo =: 3 : 0
  OpenTag 'doStatement'

  Write Advance ''
  CompileSubroutineCall ''
  Write Advance ''

  CloseTag 'doStatement'
)

NB. Compile return statement
CompileReturn =: 3 : 0
  OpenTag 'returnStatement'

  Write Advance ''

  if. -. IsSymbol ';' do.
    CompileExpression ''
  end.

  Write Advance ''

  CloseTag 'returnStatement'
)

NB. ------- Declarations -------

NB. Compile var declaration
CompileVarDec =: 3 : 0
  OpenTag 'varDec'

  while. -. IsSymbol ';' do.
    Write Advance ''
  end.

  Write Advance ''

  CloseTag 'varDec'
)

NB. Compile class variable declaration
CompileClassVarDec =: 3 : 0
  OpenTag 'classVarDec'

  while. -. IsSymbol ';' do.
    Write Advance ''
  end.

  Write Advance ''

  CloseTag 'classVarDec'
)

NB. Compile parameter list
CompileParameterList =: 3 : 0
  OpenTag 'parameterList'

  while. -. IsSymbol ')' do.
    Write Advance ''
  end.

  CloseTag 'parameterList'
)

NB. Compile subroutine body
CompileSubroutineBody =: 3 : 0
  OpenTag 'subroutineBody'

  Write Advance ''

  while. IsKeyword 'var' do.
    CompileVarDec ''
  end.

  CompileStatements ''

  Write Advance ''

  CloseTag 'subroutineBody'
)

NB. Compile subroutine declaration
CompileSubroutine =: 3 : 0
  OpenTag 'subroutineDec'

  Write Advance ''
  Write Advance ''
  Write Advance ''
  Write Advance ''

  CompileParameterList ''

  Write Advance ''

  CompileSubroutineBody ''

  CloseTag 'subroutineDec'
)

NB. ------- Class -------

NB. Compile class structure
CompileClass =: 3 : 0
  OpenTag 'class'

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

  CloseTag 'class'
)

NB. Startup message
smoutput 'parser.ijs loaded.'