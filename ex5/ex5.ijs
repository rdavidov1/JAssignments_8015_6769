NB. ex5.ijs - Project 11
NB. ============================================================

NB. This path is for Ravid
NB. load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. This path is for Hagit
load '/Users/hagitassulin/DesignPatterns/J-Assignments/ex4/ex4.ijs'


NB. ============================================================
NB. Project 11 - VM Code Generation
NB. ============================================================

vmOut =: ''
className =: ''
subroutineName =: ''
subroutineType =: ''
labelCounter =: 0

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

classTable =: 0 4 $ <''
subTable =: 0 4 $ <''

staticCount =: 0
fieldCount =: 0
argCount =: 0
varCount =: 0

StartSubroutine =: 3 : 0
  subTable =: 0 4 $ <''
  argCount =: 0
  varCount =: 0
)

Define =: 4 : 0
  NB. x = name ; y = type kind
  name =. x
  type =. > 0 { y
  kind =. > 1 { y

  if. kind -: 'static' do.
    index =. staticCount
    staticCount =: staticCount + 1
    classTable =: classTable , name ; type ; kind ; index
  elseif. kind -: 'field' do.
    index =. fieldCount
    fieldCount =: fieldCount + 1
    classTable =: classTable , name ; type ; kind ; index
  elseif. kind -: 'argument' do.
    index =. argCount
    argCount =: argCount + 1
    subTable =: subTable , name ; type ; kind ; index
  elseif. kind -: 'var' do.
    index =. varCount
    varCount =: varCount + 1
    subTable =: subTable , name ; type ; kind ; index
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

NB. ============================================================
NB. Symbol Table lookup functions
NB. ============================================================

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
  if. y -: '+' do. WriteArithmetic 'add'
  elseif. y -: '-' do. WriteArithmetic 'sub'
  elseif. y -: '*' do. 'Math.multiply' WriteCall 2
  elseif. y -: '/' do. 'Math.divide' WriteCall 2
  elseif. y -: '&amp;' do. WriteArithmetic 'and'
  elseif. y -: '&' do. WriteArithmetic 'and'
  elseif. y -: '|' do. WriteArithmetic 'or'
  elseif. y -: '&lt;' do. WriteArithmetic 'lt'
  elseif. y -: '<' do. WriteArithmetic 'lt'
  elseif. y -: '&gt;' do. WriteArithmetic 'gt'
  elseif. y -: '>' do. WriteArithmetic 'gt'
  elseif. y -: '=' do. WriteArithmetic 'eq'
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
NB. Helper - extract token value from XML token
NB. ============================================================

GetTokenValue =: 3 : 0

  tok =. y

  gt =. tok i. '>'

  rest =. (gt+1) }. tok

  lt =. rest i. '<'

  dltb lt {. rest

)


NB. ============================================================
NB. Helper - identifier info XML line
NB. ============================================================

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
NB. Stage 1 - classVarDec
NB. ============================================================

CompileClassVarDec =: 3 : 0

  out =. 2 TagLine '<classVarDec>'

  kind =. GetTokenValue CurrentToken ''
  out =. out , 4 Consume ''

  type =. GetTokenValue CurrentToken ''
  out =. out , 4 Consume ''

  name =. GetTokenValue CurrentToken ''
  name Define (type ; kind)
  out =. out , 4 DeclaredIdentifierInfoLine name
  out =. out , 4 Consume ''

  while. TokenIs ',' do.
      out =. out , 4 Consume ''

      name =. GetTokenValue CurrentToken ''
      name Define (type ; kind)
      out =. out , 4 DeclaredIdentifierInfoLine name
      out =. out , 4 Consume ''
  end.

  out =. out , 4 Consume ''
  out =. out , 2 TagLine '</classVarDec>'

  out
)

NB. ============================================================
NB. Stage 1 - varDec
NB. ============================================================

CompileVarDec =: 3 : 0

  out =. y TagLine '<varDec>'

  out =. out , (y + 2) Consume ''

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
NB. Stage 1 - parameterList
NB. ============================================================

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

NB. ============================================================
NB. Stage 1 - subroutine
NB. ============================================================

CompileSubroutine =: 3 : 0

  StartSubroutine ''

  out =. 2 TagLine '<subroutineDec>'

  subroutineType =: GetTokenValue CurrentToken ''

  if. subroutineType -: 'method' do.
    'this' Define (className ; 'argument')
  end.

  out =. out , 4 Consume ''

  out =. out , 4 Consume ''

  subroutineName =: GetTokenValue CurrentToken ''
  out =. out , 4 Consume ''

  out =. out , 4 Consume ''

  out =. out , CompileParameterList 4

  out =. out , 4 Consume ''

  out =. out , CompileSubroutineBody 4

  out =. out , 2 TagLine '</subroutineDec>'

  out
)


CompileSubroutineBody =: 3 : 0

  out =. y TagLine '<subroutineBody>'

  out =. out , (y + 2) Consume ''

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

  out =. out , (y + 2) Consume ''

  out =. out , y TagLine '</subroutineBody>'

  out
)

NB. ============================================================
NB. Stage 1 - letStatement identifier use
NB. ============================================================

CompileLet =: 3 : 0

  out =. y TagLine '<letStatement>'

  out =. out , (y + 2) Consume ''

  name =. GetTokenValue CurrentToken ''
  out =. out , (y + 2) IdentifierInfoLine name
  out =. out , (y + 2) Consume ''

  if. TokenIs '[' do.
    out =. out , (y + 2) Consume ''
    out =. out , CompileExpression (y + 2)
    out =. out , (y + 2) Consume ''
  end.

  out =. out , (y + 2) Consume ''
  out =. out , CompileExpression (y + 2)
  out =. out , (y + 2) Consume ''

  out =. out , y TagLine '</letStatement>'

  out
)

NB. ============================================================
NB. Stage 1 - term identifier use
NB. ============================================================

CompileTerm =: 3 : 0

  out =. y TagLine '<term>'

  if. TokenIs '(' do.

    out =. out , (y + 2) Consume ''
    out =. out , CompileExpression (y + 2)
    out =. out , (y + 2) Consume ''

  elseif. (TokenIs '-') +. (TokenIs '~') do.

    out =. out , (y + 2) Consume ''
    out =. out , CompileTerm (y + 2)

  elseif. IsIdentifier '' do.

    name =. GetTokenValue CurrentToken ''

    if. NextTokenIs '[' do.

      out =. out , (y + 2) IdentifierInfoLine name
      out =. out , (y + 2) Consume ''
      out =. out , (y + 2) Consume ''
      out =. out , CompileExpression (y + 2)
      out =. out , (y + 2) Consume ''

    elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.

      out =. out , CompileSubroutineCall (y + 2)

    else.

      out =. out , (y + 2) IdentifierInfoLine name
      out =. out , (y + 2) Consume ''

    end.

  else.

    out =. out , (y + 2) Consume ''

  end.

  out =. out , y TagLine '</term>'

  out
)

NB. ============================================================
NB. Stage 1 - subroutineCall identifiers
NB. ============================================================

CompileSubroutineCall =: 3 : 0

  out =. ''

  firstName =. GetTokenValue CurrentToken ''

  if. NextTokenIs '.' do.

    if. -. (KindOf firstName) -: 'none' do.
      out =. out , y IdentifierInfoLine firstName
    else.
      out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="class" index="-1" usage="used" />' , LF
    end.

    out =. out , y Consume ''
    out =. out , y Consume ''

    secondName =. GetTokenValue CurrentToken ''
    out =. out , (Spaces y) , '<identifierInfo name="' , secondName , '" category="subroutine" index="-1" usage="used" />' , LF
    out =. out , y Consume ''

  else.

    out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="subroutine" index="-1" usage="used" />' , LF
    out =. out , y Consume ''

  end.

  out =. out , y Consume ''
  out =. out , CompileExpressionList y
  out =. out , y Consume ''

  out
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

smoutput 'ex5 loaded.'