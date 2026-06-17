NB. ex5.ijs - Project 11 - Jack Compiler VM Code Generation
NB. ============================================================
NB. This file intentionally REUSES Project 10 infrastructure:
NB.   AnalyzeFile, ReadParserTokens, ParseFile, Consume, TokenIs, etc.
NB. Project 11 changes the CompileXXX functions so the same parsing flow
NB. also writes VM code into Xxx.vm.
NB. ============================================================

NB. This path is for Ravid
NB. load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. This path is for Hagit
load '/Users/hagitassulin/DesignPatterns/J-Assignments/ex4/ex4.ijs'

NB. ============================================================
NB. Project 11 - Global state
NB. ============================================================

vmOut =: ''
className =: ''
subroutineName =: ''
subroutineType =: ''
labelCounter =: 0
expressionCount =: 0

NB. ============================================================
NB. VM Writer
NB. ============================================================

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

WriteLabel =: 3 : 0
  WriteVM 'label ' , y
)

WriteGoto =: 3 : 0
  WriteVM 'goto ' , y
)

WriteIf =: 3 : 0
  WriteVM 'if-goto ' , y
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
NB. Symbol Table
NB. ============================================================

classTable =: ''
subTable =: ''

staticCount =: 0
fieldCount =: 0
argCount =: 0
varCount =: 0

StartSubroutine =: 3 : 0
  subTable =: ''
  argCount =: 0
  varCount =: 0
)

Define =: 4 : 0
  NB. x = name ; y = type ; kind
  name =. x
  type =. > 0 { y
  kind =. > 1 { y

  if. kind -: 'static' do.
    index =. staticCount
    staticCount =: staticCount + 1
    classTable =: classTable , < name ; type ; kind ; index
  elseif. kind -: 'field' do.
    index =. fieldCount
    fieldCount =: fieldCount + 1
    classTable =: classTable , < name ; type ; kind ; index
  elseif. kind -: 'argument' do.
    index =. argCount
    argCount =: argCount + 1
    subTable =: subTable , < name ; type ; kind ; index
  elseif. kind -: 'var' do.
    index =. varCount
    varCount =: varCount + 1
    subTable =: subTable , < name ; type ; kind ; index
  end.
)

VarCountKind =: 3 : 0
  if. y -: 'static' do. staticCount
  elseif. y -: 'field' do. fieldCount
  elseif. y -: 'argument' do. argCount
  elseif. y -: 'var' do. varCount
  else. 0
  end.
)

FindInTable =: 4 : 0
  table =. x
  name =. y

  for_i. i. # table do.
    row =. i { table
    if. name -: > 0 { row do.
      row return.
    end.
  end.

  ''
)

FindSymbol =: 3 : 0
  name =. y

  result =. subTable FindInTable name
  if. -. result -: '' do.
    result return.
  end.

  result =. classTable FindInTable name
  if. -. result -: '' do.
    result return.
  end.

  ''
)

KindOf =: 3 : 0
  row =. FindSymbol y
  if. row -: '' do.
    'none'
  else.
    > 2 { row
  end.
)

TypeOf =: 3 : 0
  row =. FindSymbol y
  if. row -: '' do.
    ''
  else.
    > 1 { row
  end.
)

IndexOf =: 3 : 0
  row =. FindSymbol y
  if. row -: '' do.
    _1
  else.
    > 3 { row
  end.
)

KindToSegment =: 3 : 0
  if. y -: 'static' do.
    'static'
  elseif. y -: 'field' do.
    'this'
  elseif. y -: 'argument' do.
    'argument'
  elseif. y -: 'var' do.
    'local'
  else.
    ''
  end.
)

PushVar =: 3 : 0
  name =. y
  seg =. KindToSegment KindOf name
  idx =. IndexOf name
  seg WritePush idx
)

PopVar =: 3 : 0
  name =. y
  seg =. KindToSegment KindOf name
  idx =. IndexOf name
  seg WritePop idx
)

WriteOp =: 3 : 0
  
  op =. Clean y
  if. op -: '+' do. WriteArithmetic 'add'
  elseif. op -: '-' do. WriteArithmetic 'sub'
  elseif. op -: '*' do. 'Math.multiply' WriteCall 2
  elseif. op -: '/' do. 'Math.divide' WriteCall 2
  elseif. op -: '&amp;' do. WriteArithmetic 'and'
  elseif. op -: '&' do. WriteArithmetic 'and'
  elseif. op -: '|' do. WriteArithmetic 'or'
  elseif. op -: '&lt;' do. WriteArithmetic 'lt'
  elseif. op -: '<' do. WriteArithmetic 'lt'
  elseif. op -: '&gt;' do. WriteArithmetic 'gt'
  elseif. op -: '>' do. WriteArithmetic 'gt'
  elseif. op -: '=' do. WriteArithmetic 'eq'
  end.
)

NewLabel =: 3 : 0
  label =. y , '_' , ": labelCounter
  labelCounter =: labelCounter + 1
  label
)

VmPath =: 3 : 0
  (_5 }. y) , '.vm'
)

NB. ============================================================
NB. Helpers
NB. ============================================================

GetTokenValue =: 3 : 0
  tok =. y
  gt =. tok i. '>'
  rest =. (gt + 1) }. tok
  lt =. rest i. '<'
  dltb lt {. rest
)
Clean =: 3 : 0
  dltb y
)
IsOp =: 3 : 0
  op =. dltb GetTokenValue CurrentToken ''
  (op -: '+') +. (op -: '-') +. (op -: '*') +. (op -: '/') +. (op -: '&amp;') +. (op -: '&') +. (op -: '|') +. (op -: '&lt;') +. (op -: '<') +. (op -: '&gt;') +. (op -: '>') +. (op -: '=')
)

IdentifierInfoLine =: 4 : 0
  indent =. x
  name =. y
  kind =. KindOf name
  index =. IndexOf name
  (Spaces indent) , '<identifierInfo name="' , name , '" category="' , kind , '" index="' , (": index) , '" usage="used" />' , LF
)

DeclaredIdentifierInfoLine =: 4 : 0
  indent =. x
  name =. y
  kind =. KindOf name
  index =. IndexOf name
  (Spaces indent) , '<identifierInfo name="' , name , '" category="' , kind , '" index="' , (": index) , '" usage="declared" />' , LF
)

NB. ============================================================
NB. class
NB. ============================================================

CompileClass =: 3 : 0
  NB. Important: this keeps the Project 10 ParseFile flow.
  NB. ParseFile calls CompileClass tokenLines, so Project 11 receives y here.
  tokens_list =: y
  pointer =: 0

  classTable =: ''
  staticCount =: 0
  fieldCount =: 0

  out =. 0 TagLine '<class>'

  out =. out , 2 Consume ''  NB. class

  className =: GetTokenValue CurrentToken ''
  out =. out , 2 Consume ''  NB. className

  out =. out , 2 Consume ''  NB. {

  while. (TokenIs 'static') +. (TokenIs 'field') do.
    out =. out , CompileClassVarDec 2
  end.

  while. (TokenIs 'constructor') +. (TokenIs 'function') +. (TokenIs 'method') do.
    out =. out , CompileSubroutine 2
  end.

  out =. out , 2 Consume ''  NB. }
  out =. out , 0 TagLine '</class>'
  out
)

NB. ============================================================
NB. classVarDec
NB. ============================================================

CompileClassVarDec =: 3 : 0
  out =. y TagLine '<classVarDec>'

  kind =. GetTokenValue CurrentToken ''
  out =. out , (y + 2) Consume ''

  type =. GetTokenValue CurrentToken ''
  out =. out , (y + 2) Consume ''

  name =. GetTokenValue CurrentToken ''
  name Define (type ; kind)
  out =. out , (y + 2) DeclaredIdentifierInfoLine name
  out =. out , (y + 2) Consume ''

  while. TokenIs ',' do.
    out =. out , (y + 2) Consume ''

    name =. GetTokenValue CurrentToken ''
    name Define (type ; kind)
    out =. out , (y + 2) DeclaredIdentifierInfoLine name
    out =. out , (y + 2) Consume ''
  end.

  out =. out , (y + 2) Consume ''
  out =. out , y TagLine '</classVarDec>'
  out
)

NB. ============================================================
NB. subroutineDec / parameterList / varDec / subroutineBody
NB. ============================================================

CompileSubroutine =: 3 : 0
  StartSubroutine ''

  out =. y TagLine '<subroutineDec>'

  subroutineType =: GetTokenValue CurrentToken ''
  out =. out , (y + 2) Consume ''

  out =. out , (y + 2) Consume ''  NB. return type

  subroutineName =: GetTokenValue CurrentToken ''
  out =. out , (y + 2) Consume ''

  if. subroutineType -: 'method' do.
    'this' Define (className ; 'argument')
  end.

  out =. out , (y + 2) Consume ''  NB. (
  out =. out , CompileParameterList (y + 2)
  out =. out , (y + 2) Consume ''  NB. )

  out =. out , CompileSubroutineBody (y + 2)

  out =. out , y TagLine '</subroutineDec>'
  out
)

CompileParameterList =: 3 : 0
  out =. y TagLine '<parameterList>'

  if. -. TokenIs ')' do.
    type =. GetTokenValue CurrentToken ''
    out =. out , (y + 2) Consume ''

    name =. GetTokenValue CurrentToken ''
    name Define (type ; 'argument')
    out =. out , (y + 2) DeclaredIdentifierInfoLine name
    out =. out , (y + 2) Consume ''

    while. TokenIs ',' do.
      out =. out , (y + 2) Consume ''

      type =. GetTokenValue CurrentToken ''
      out =. out , (y + 2) Consume ''

      name =. GetTokenValue CurrentToken ''
      name Define (type ; 'argument')
      out =. out , (y + 2) DeclaredIdentifierInfoLine name
      out =. out , (y + 2) Consume ''
    end.
  end.

  out =. out , y TagLine '</parameterList>'
  out
)

CompileSubroutineBody =: 3 : 0
  out =. y TagLine '<subroutineBody>'

  out =. out , (y + 2) Consume ''  NB. {

  while. TokenIs 'var' do.
    out =. out , CompileVarDec (y + 2)
  end.

  fullName =. className , '.' , subroutineName
  fullName WriteFunction varCount

  if. subroutineType -: 'constructor' do.
    'constant' WritePush fieldCount
    'Memory.alloc' WriteCall 1
    'pointer' WritePop 0
  elseif. subroutineType -: 'method' do.
    'argument' WritePush 0
    'pointer' WritePop 0
  end.

  out =. out , CompileStatements (y + 2)

  out =. out , (y + 2) Consume ''  NB. }
  out =. out , y TagLine '</subroutineBody>'
  out
)

CompileVarDec =: 3 : 0
  out =. y TagLine '<varDec>'

  out =. out , (y + 2) Consume ''  NB. var

  type =. GetTokenValue CurrentToken ''
  out =. out , (y + 2) Consume ''

  name =. GetTokenValue CurrentToken ''
  name Define (type ; 'var')
  out =. out , (y + 2) DeclaredIdentifierInfoLine name
  out =. out , (y + 2) Consume ''

  while. TokenIs ',' do.
    out =. out , (y + 2) Consume ''

    name =. GetTokenValue CurrentToken ''
    name Define (type ; 'var')
    out =. out , (y + 2) DeclaredIdentifierInfoLine name
    out =. out , (y + 2) Consume ''
  end.

  out =. out , (y + 2) Consume ''
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

CompileLet =: 3 : 0
  out =. y TagLine '<letStatement>'

  out =. out , (y + 2) Consume ''  NB. let

  name =. GetTokenValue CurrentToken ''
  out =. out , (y + 2) IdentifierInfoLine name
  out =. out , (y + 2) Consume ''

  isArray =. 0

  if. TokenIs '[' do.
    isArray =. 1
    PushVar name
    out =. out , (y + 2) Consume ''
    out =. out , CompileExpression (y + 2)
    WriteArithmetic 'add'
    out =. out , (y + 2) Consume ''
  end.

  out =. out , (y + 2) Consume ''  NB. =
  out =. out , CompileExpression (y + 2)

  if. isArray do.
    'temp' WritePop 0
    'pointer' WritePop 1
    'temp' WritePush 0
    'that' WritePop 0
  else.
    PopVar name
  end.

  out =. out , (y + 2) Consume ''  NB. ;
  out =. out , y TagLine '</letStatement>'
  out
)

CompileDo =: 3 : 0
  out =. y TagLine '<doStatement>'

  out =. out , (y + 2) Consume ''  NB. do
  out =. out , CompileSubroutineCall (y + 2)
  'temp' WritePop 0
  out =. out , (y + 2) Consume ''  NB. ;

  out =. out , y TagLine '</doStatement>'
  out
)

CompileReturn =: 3 : 0
  out =. y TagLine '<returnStatement>'

  out =. out , (y + 2) Consume ''  NB. return

  if. TokenIs ';' do.
    'constant' WritePush 0
  else.
    out =. out , CompileExpression (y + 2)
  end.

  WriteReturn ''

  out =. out , (y + 2) Consume ''  NB. ;
  out =. out , y TagLine '</returnStatement>'
  out
)

CompileIf =: 3 : 0
  out =. y TagLine '<ifStatement>'

  falseLabel =. NewLabel 'IF_FALSE'
  endLabel =. NewLabel 'IF_END'

  out =. out , (y + 2) Consume ''  NB. if
  out =. out , (y + 2) Consume ''  NB. (
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''  NB. )

  WriteArithmetic 'not'
  WriteIf falseLabel

  out =. out , (y + 2) Consume ''  NB. {
  out =. out , CompileStatements (y + 2)
  out =. out , (y + 2) Consume ''  NB. }

  if. TokenIs 'else' do.
    WriteGoto endLabel
    WriteLabel falseLabel

    out =. out , (y + 2) Consume ''  NB. else
    out =. out , (y + 2) Consume ''  NB. {
    out =. out , CompileStatements (y + 2)
    out =. out , (y + 2) Consume ''  NB. }

    WriteLabel endLabel
  else.
    WriteLabel falseLabel
  end.

  out =. out , y TagLine '</ifStatement>'
  out
)

CompileWhile =: 3 : 0
  out =. y TagLine '<whileStatement>'

  expLabel =. NewLabel 'WHILE_EXP'
  endLabel =. NewLabel 'WHILE_END'

  WriteLabel expLabel

  out =. out , (y + 2) Consume ''  NB. while
  out =. out , (y + 2) Consume ''  NB. (
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''  NB. )

  WriteArithmetic 'not'
  WriteIf endLabel

  out =. out , (y + 2) Consume ''  NB. {
  out =. out , CompileStatements (y + 2)
  out =. out , (y + 2) Consume ''  NB. }

  WriteGoto expLabel
  WriteLabel endLabel

  out =. out , y TagLine '</whileStatement>'
  out
)

NB. ============================================================
NB. expressions / terms / calls
NB. ============================================================

CompileExpression =: 3 : 0
  out =. y TagLine '<expression>'

  out =. out , CompileTerm (y + 2)

  while. IsOp '' do.
    op =. dltb GetTokenValue CurrentToken ''
    out =. out , (y + 2) Consume ''
    out =. out , CompileTerm (y + 2)
    WriteOp op
  end.

  out =. out , y TagLine '</expression>'
  out
)

CompileTerm =: 3 : 0
  out =. y TagLine '<term>'

  tokVal =. GetTokenValue CurrentToken ''

  if. TokenIs '(' do.
    out =. out , (y + 2) Consume ''
    out =. out , CompileExpression (y + 2)
    out =. out , (y + 2) Consume ''

  elseif. TokenIs '-' do.
    out =. out , (y + 2) Consume ''
    out =. out , CompileTerm (y + 2)
    WriteArithmetic 'neg'

  elseif. TokenIs '~' do.
    out =. out , (y + 2) Consume ''
    out =. out , CompileTerm (y + 2)
    WriteArithmetic 'not'

  elseif. IsIdentifier '' do.
    name =. tokVal

    if. NextTokenIs '[' do.
      PushVar name
      out =. out , (y + 2) IdentifierInfoLine name
      out =. out , (y + 2) Consume ''  NB. array name
      out =. out , (y + 2) Consume ''  NB. [
      out =. out , CompileExpression (y + 2)
      WriteArithmetic 'add'
      'pointer' WritePop 1
      'that' WritePush 0
      out =. out , (y + 2) Consume ''  NB. ]

    elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.
      out =. out , CompileSubroutineCall (y + 2)

    else.
      PushVar name
      out =. out , (y + 2) IdentifierInfoLine name
      out =. out , (y + 2) Consume ''
    end.

  elseif. CurrentHas '<integerConstant>' do.
    'constant' WritePush ". tokVal
    out =. out , (y + 2) Consume ''

  elseif. CurrentHas '<stringConstant>' do.
    'constant' WritePush # tokVal
    'String.new' WriteCall 1
    for_ch. tokVal do.
      'constant' WritePush a. i. ch
      'String.appendChar' WriteCall 2
    end.
    out =. out , (y + 2) Consume ''

  elseif. tokVal -: 'true' do.
    'constant' WritePush 0
    WriteArithmetic 'not'
    out =. out , (y + 2) Consume ''

  elseif. tokVal -: 'false' do.
    'constant' WritePush 0
    out =. out , (y + 2) Consume ''

  elseif. tokVal -: 'null' do.
    'constant' WritePush 0
    out =. out , (y + 2) Consume ''

  elseif. tokVal -: 'this' do.
    'pointer' WritePush 0
    out =. out , (y + 2) Consume ''

  else.
    out =. out , (y + 2) Consume ''
  end.

  out =. out , y TagLine '</term>'
  out
)

CompileExpressionList =: 3 : 0
  out =. y TagLine '<expressionList>'

  if. -. TokenIs ')' do.
    out =. out , CompileExpression (y + 2)
    expressionCount =: expressionCount + 1

    while. TokenIs ',' do.
      out =. out , (y + 2) Consume ''
      out =. out , CompileExpression (y + 2)
      expressionCount =: expressionCount + 1
    end.
  end.

  out =. out , y TagLine '</expressionList>'
  out
)

CompileSubroutineCall =: 3 : 0
  out =. ''
  nArgs =. 0

  firstName =. GetTokenValue CurrentToken ''

  if. NextTokenIs '.' do.
    if. -. (KindOf firstName) -: 'none' do.
      PushVar firstName
      nArgs =. nArgs + 1
      callName =. (TypeOf firstName) , '.'
      out =. out , y IdentifierInfoLine firstName
    else.
      callName =. firstName , '.'
      out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="class" index="-1" usage="used" />' , LF
    end.

    out =. out , y Consume ''  NB. class/var name
    out =. out , y Consume ''  NB. .

    secondName =. GetTokenValue CurrentToken ''
    callName =. callName , secondName
    out =. out , (Spaces y) , '<identifierInfo name="' , secondName , '" category="subroutine" index="-1" usage="used" />' , LF
    out =. out , y Consume ''

  else.
    'pointer' WritePush 0
    nArgs =. nArgs + 1
    callName =. className , '.' , firstName

    out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="subroutine" index="-1" usage="used" />' , LF
    out =. out , y Consume ''
  end.

  out =. out , y Consume ''  NB. (

  beforeArgs =. expressionCount
  expressionCount =: 0
  out =. out , CompileExpressionList y
  nArgs =. nArgs + expressionCount
  expressionCount =: beforeArgs

  out =. out , y Consume ''  NB. )

  callName WriteCall nArgs
  out
)

NB. ============================================================
NB. Main
NB. ============================================================

Main =: 3 : 0
  path =. y

  if. '.jack' -: _5 {. path do.
    vmOut =: ''
    labelCounter =: 0
    ParseFile path
    outPath =. VmPath path
    vmOut WriteFile outPath
    smoutput 'Written VM: ' , outPath
    return.
  end.

  files =. GetJackFiles path

  if. 0 = # files do.
    smoutput 'No .jack files found in: ' , path
    return.
  end.

  for_f. files do.
    vmOut =: ''
    labelCounter =: 0
    ParseFile > f
    outPath =. VmPath > f
    vmOut WriteFile outPath
    smoutput 'Written VM: ' , outPath
  end.

  0
)

smoutput 'ex5 loaded.'
