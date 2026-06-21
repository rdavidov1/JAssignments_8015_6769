NB. ex5.ijs - Project 11
NB. ============================================================

NB. This path is for Ravid
load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. This path is for Hagit
NB.load '/Users/hagitassulin/DesignPatterns/J-Assignments/ex4/ex4.ijs'

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

NB. ============================================================
NB. Stage 1 - letStatement identifier use
NB. ============================================================

CompileLet =: 3 : 0

  out =. y TagLine '<letStatement>'

  out =. out , (y + 2) Consume ''   NB. let

  name =. GetTokenValue CurrentToken ''

  kind =. KindOf name

  segment =. KindToSegment kind

  index =. IndexOf name

  out =. out , (y + 2) IdentifierInfoLine name

  out =. out , (y + 2) Consume ''   NB. varName

  isArray =. 0

  if. TokenIs '[' do.

    isArray =. 1

    segment WritePush index

    out =. out , (y + 2) Consume ''   NB. [

    out =. out , CompileExpression (y + 2)

    out =. out , (y + 2) Consume ''   NB. ]

    WriteArithmetic 'add'

  end.

  out =. out , (y + 2) Consume ''   NB. =

  out =. out , CompileExpression (y + 2)

  if. isArray do.

    'temp' WritePop 0

    'pointer' WritePop 1

    'temp' WritePush 0

    'that' WritePop 0

  else.

    segment WritePop index

  end.

  out =. out , (y + 2) Consume ''   NB. ;

  out =. out , y TagLine '</letStatement>'

  out

)
CompileIf =: 3 : 0

  out =. y TagLine '<ifStatement>'

  trueLabel =. 'IF_TRUE' , ": labelCounter
  falseLabel =. 'IF_FALSE' , ": labelCounter
  endLabel =. 'IF_END' , ": labelCounter

  labelCounter =: labelCounter + 1

  out =. out , (y + 2) Consume ''   NB. if

  out =. out , (y + 2) Consume ''   NB. (

  out =. out , CompileExpression (y + 2)

  WriteIf trueLabel

  WriteGoto falseLabel

  WriteLabel trueLabel

  out =. out , (y + 2) Consume ''   NB. )

  out =. out , (y + 2) Consume ''   NB. {

  out =. out , CompileStatements (y + 2)

  out =. out , (y + 2) Consume ''   NB. }

  if. TokenIs 'else' do.

    WriteGoto endLabel

    WriteLabel falseLabel

    out =. out , (y + 2) Consume ''   NB. else

    out =. out , (y + 2) Consume ''   NB. {

    out =. out , CompileStatements (y + 2)

    out =. out , (y + 2) Consume ''   NB. }

    WriteLabel endLabel

  else.

    WriteLabel falseLabel

  end.

  out =. out , y TagLine '</ifStatement>'

  out

)

CompileWhile =: 3 : 0

  out =. y TagLine '<whileStatement>'

  expLabel =. 'WHILE_EXP' , ": labelCounter
  endLabel =. 'WHILE_END' , ": labelCounter

  labelCounter =: labelCounter + 1

  WriteLabel expLabel

  out =. out , (y + 2) Consume ''   NB. while

  out =. out , (y + 2) Consume ''   NB. (

  out =. out , CompileExpression (y + 2)

  WriteArithmetic 'not'

  WriteIf endLabel

  out =. out , (y + 2) Consume ''   NB. )

  out =. out , (y + 2) Consume ''   NB. {

  out =. out , CompileStatements (y + 2)

  out =. out , (y + 2) Consume ''   NB. }

  WriteGoto expLabel

  WriteLabel endLabel

  out =. out , y TagLine '</whileStatement>'

  out

)


NB. ============================================================
NB. Stage 1 - subroutineCall identifiers
NB. ============================================================


ParseFile =: 3 : 0

  inputFile =. y

  AnalyzeFile inputFile

  base =. ((# inputFile) - 5) {. inputFile

  tokenFile =. base , 'T.xml'
  outputFile =. base , '.xml'
  vmFile =. base , '.vm'

  vmOut =: ''

  tokenLines =. ReadParserTokens tokenFile

  finalXml =. CompileClass tokenLines

  finalXml WriteFile outputFile

  vmOut WriteFile vmFile

  smoutput 'Written: ' , outputFile
  smoutput 'Written: ' , vmFile

)


expressionListCount =: 0

CompileClass =: 3 : 0
  classTable =: 0 4 $ <''
  staticCount =: 0
  fieldCount =: 0
  tokens_list =: y
  pointer =: 0

  out =. '<class>' , LF

  out =. out , 2 Consume ''  NB. class

  className =: GetTokenValue CurrentToken ''
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

CompileSubroutineBody =: 3 : 0
  out =. y TagLine '<subroutineBody>'

  out =. out , (y + 2) Consume ''  NB. {

  while. TokenIs 'var' do.
    out =. out , CompileVarDec (y + 2)
  end.

  funcName =. className , '.' , subroutineName
    funcName WriteFunction varCount

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

CompileExpression =: 3 : 0

  out =. y TagLine '<expression>'

  out =. out , CompileTerm (y + 2)

  while. IsOp '' do.

      op =. GetTokenValue CurrentToken ''

      smoutput 'OP=' , op

      out =. out , (y + 2) Consume ''

      out =. out , CompileTerm (y + 2)

      if. '+' e. op do.

          smoutput 'ADDING'

          WriteArithmetic 'add'

      elseif. '-' e. op do.

          WriteArithmetic 'sub'

      elseif. '*' e. op do.

          smoutput 'MULT'

          'Math.multiply' WriteCall 2

      elseif. '/' e. op do.

          'Math.divide' WriteCall 2

      elseif. '&amp;' E. op do.

          WriteArithmetic 'and'

      elseif. '|' e. op do.

          WriteArithmetic 'or'

      elseif. '&lt;' E. op do.

          WriteArithmetic 'lt'

      elseif. '&gt;' E. op do.

          WriteArithmetic 'gt'

      elseif. '=' e. op do.

          WriteArithmetic 'eq'

      end.

  end.

  out =. out , y TagLine '</expression>'

  out

)

CompileTerm =: 3 : 0

  out =. y TagLine '<term>'

  if. TokenIs '(' do.

    out =. out , (y + 2) Consume ''

    out =. out , CompileExpression (y + 2)

    out =. out , (y + 2) Consume ''

  elseif. (TokenIs '-') +. (TokenIs '~') do.

    op =. GetTokenValue CurrentToken ''

    out =. out , (y + 2) Consume ''

    out =. out , CompileTerm (y + 2)

    if. '-' e. op do.

      WriteArithmetic 'neg'

    else.

      WriteArithmetic 'not'

    end.

  elseif. IsIdentifier '' do.

    if. NextTokenIs '[' do.

      name =. GetTokenValue CurrentToken ''

      kind =. KindOf name

      segment =. KindToSegment kind

      index =. IndexOf name

      segment WritePush index

      out =. out , (y + 2) Consume ''   NB. varName

      out =. out , (y + 2) Consume ''   NB. [

      out =. out , CompileExpression (y + 2)

      out =. out , (y + 2) Consume ''   NB. ]

      WriteArithmetic 'add'

      'pointer' WritePop 1

      'that' WritePush 0

    elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.

      out =. out , CompileSubroutineCall (y + 2)

    else.

      name =. GetTokenValue CurrentToken ''

      kind =. KindOf name

      segment =. KindToSegment kind

      index =. IndexOf name

      segment WritePush index

      out =. out , (y + 2) Consume ''

    end.

  else.

    value =. GetTokenValue CurrentToken ''

    if. CurrentHas '<integerConstant>' do.

      'constant' WritePush ". value

    elseif. CurrentHas '<stringConstant>' do.

      'constant' WritePush # value

      'String.new' WriteCall 1

      for_ch. value do.

        'constant' WritePush a. i. ch

        'String.appendChar' WriteCall 2

      end.

    elseif. CurrentHas '<keyword>' do.

      if. value -: 'true' do.

        'constant' WritePush 0

        WriteArithmetic 'not'

      elseif. value -: 'false' do.

        'constant' WritePush 0

      elseif. value -: 'null' do.

        'constant' WritePush 0

      elseif. value -: 'this' do.

        'pointer' WritePush 0

      end.

    end.

    out =. out , (y + 2) Consume ''

  end.

  out =. out , y TagLine '</term>'

  out

)

CompileExpressionList =: 3 : 0
  expressionListCount =: 0
  out =. y TagLine '<expressionList>'

  if. -. TokenIs ')' do.
    out =. out , CompileExpression (y + 2)
    expressionListCount =: expressionListCount + 1

    while. TokenIs ',' do.
      out =. out , (y + 2) Consume ''
      out =. out , CompileExpression (y + 2)
      expressionListCount =: expressionListCount + 1
    end.
  end.

  out =. out , y TagLine '</expressionList>'

  out
)

CompileSubroutineCall =: 3 : 0
  out =. ''
  nArgs =. 0

  NB. first name: subroutineName / className / varName
  firstName =. GetTokenValue CurrentToken ''
  out =. out , y Consume ''

  if. TokenIs '.' do.
    out =. out , y Consume ''  NB. consume '.'

    secondName =. GetTokenValue CurrentToken ''
    out =. out , y Consume ''

    kind =. KindOf firstName

    if. kind -: 'none' do.
      NB. ClassName.function(...)
      callName =. firstName , '.' , secondName
    else.
      NB. varName.method(...)
      segment =. KindToSegment kind
      index =. IndexOf firstName
      objType =. TypeOf firstName

      segment WritePush index
      nArgs =. 1

      callName =. objType , '.' , secondName
    end.

  else.
    NB. methodName(...) of current class
    'pointer' WritePush 0
    nArgs =. 1

    callName =. className , '.' , firstName
  end.

  out =. out , y Consume ''  NB. consume '('

  out =. out , CompileExpressionList y

  out =. out , y Consume ''  NB. consume ')'

  callName WriteCall nArgs + expressionListCount

  out
)
CompileDo =: 3 : 0
    smoutput 'ENTER DO'
  out =. y TagLine '<doStatement>'

  out =. out , (y + 2) Consume ''  NB. do
  out =. out , CompileSubroutineCall (y + 2)
  out =. out , (y + 2) Consume ''  NB. ;

  'temp' WritePop 0

  out =. out , y TagLine '</doStatement>'

  out
)

CompileReturn =: 3 : 0
    smoutput 'ENTER RETURN'
  out =. y TagLine '<returnStatement>'

  out =. out , (y + 2) Consume ''  NB. return

  if. -. TokenIs ';' do.
    out =. out , CompileExpression (y + 2)
  else.
    'constant' WritePush 0
  end.

  out =. out , (y + 2) Consume ''  NB. ;

  WriteReturn ''

  out =. out , y TagLine '</returnStatement>'

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