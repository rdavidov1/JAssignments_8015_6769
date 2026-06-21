NB. NB. ex5.ijs - Project 11
NB. NB. ============================================================

NB. load 'C:/Users/Home/Documents/temp/ex4/ex4.ijs'

NB. NB. ============================================================
NB. NB. Project 11 - VM Code Generation
NB. NB. ============================================================

NB. vmOut =: ''
NB. className =: ''
NB. subroutineName =: ''
NB. subroutineType =: ''
NB. labelCounter =: 0

NB. NB. ============================================================
NB. NB. VM Writer
NB. NB. ============================================================

NB. WriteVM =: 3 : 0
NB.   vmOut =: vmOut , y , LF
NB. )

NB. WritePush =: 4 : 0
NB.   WriteVM 'push ' , x , ' ' , ": y
NB. )

NB. WritePop =: 4 : 0
NB.   WriteVM 'pop ' , x , ' ' , ": y
NB. )

NB. WriteArithmetic =: 3 : 0
NB.   WriteVM y
NB. )

NB. WriteLabel =: 3 : 0
NB.   WriteVM 'label ' , y
NB. )

NB. WriteGoto =: 3 : 0
NB.   WriteVM 'goto ' , y
NB. )

NB. WriteIf =: 3 : 0
NB.   WriteVM 'if-goto ' , y
NB. )

NB. WriteCall =: 4 : 0
NB.   WriteVM 'call ' , x , ' ' , ": y
NB. )

NB. WriteFunction =: 4 : 0
NB.   WriteVM 'function ' , x , ' ' , ": y
NB. )

NB. WriteReturn =: 3 : 0
NB.   WriteVM 'return'
NB. )

NB. NB. ============================================================
NB. NB. Symbol Table
NB. NB. ============================================================

NB. classTable =: 0 4 $ <''
NB. subTable =: 0 4 $ <''

NB. staticCount =: 0
NB. fieldCount =: 0
NB. argCount =: 0
NB. varCount =: 0

NB. StartSubroutine =: 3 : 0
NB.   subTable =: 0 4 $ <''
NB.   argCount =: 0
NB.   varCount =: 0
NB. )

NB. Define =: 4 : 0
NB.   NB. x = name ; y = type kind
NB.   name =. x
NB.   type =. > 0 { y
NB.   kind =. > 1 { y

NB.   if. kind -: 'static' do.
NB.     index =. staticCount
NB.     staticCount =: staticCount + 1
NB.     classTable =: classTable , name ; type ; kind ; index
NB.   elseif. kind -: 'field' do.
NB.     index =. fieldCount
NB.     fieldCount =: fieldCount + 1
NB.     classTable =: classTable , name ; type ; kind ; index
NB.   elseif. kind -: 'argument' do.
NB.     index =. argCount
NB.     argCount =: argCount + 1
NB.     subTable =: subTable , name ; type ; kind ; index
NB.   elseif. kind -: 'var' do.
NB.     index =. varCount
NB.     varCount =: varCount + 1
NB.     subTable =: subTable , name ; type ; kind ; index
NB.   end.
NB. )

NB. VarCountKind =: 3 : 0
NB.   if. y -: 'static' do. staticCount
NB.   elseif. y -: 'field' do. fieldCount
NB.   elseif. y -: 'argument' do. argCount
NB.   elseif. y -: 'var' do. varCount
NB.   else. 0
NB.   end.
NB. )

NB. NB. ============================================================
NB. NB. Symbol Table lookup functions
NB. NB. ============================================================

NB. FindInTable =: 4 : 0
NB.   table =. x
NB.   name =. y

NB.   for_i. i. # table do.
NB.     row =. i { table
NB.     if. name -: > 0 { row do.
NB.       row return.
NB.     end.
NB.   end.

NB.   ''
NB. )

NB. FindSymbol =: 3 : 0
NB.   name =. y

NB.   result =. subTable FindInTable name
NB.   if. -. result -: '' do.
NB.     result return.
NB.   end.

NB.   result =. classTable FindInTable name
NB.   if. -. result -: '' do.
NB.     result return.
NB.   end.

NB.   ''
NB. )

NB. KindOf =: 3 : 0
NB.   row =. FindSymbol y
NB.   if. row -: '' do.
NB.     'none'
NB.   else.
NB.     > 2 { row
NB.   end.
NB. )

NB. TypeOf =: 3 : 0
NB.   row =. FindSymbol y
NB.   if. row -: '' do.
NB.     ''
NB.   else.
NB.     > 1 { row
NB.   end.
NB. )

NB. IndexOf =: 3 : 0
NB.   row =. FindSymbol y
NB.   if. row -: '' do.
NB.     _1
NB.   else.
NB.     > 3 { row
NB.   end.
NB. )

NB. KindToSegment =: 3 : 0
NB.   if. y -: 'static' do.
NB.     'static'
NB.   elseif. y -: 'field' do.
NB.     'this'
NB.   elseif. y -: 'argument' do.
NB.     'argument'
NB.   elseif. y -: 'var' do.
NB.     'local'
NB.   else.
NB.     ''
NB.   end.
NB. )

NB. NB. ============================================================
NB. NB. Helper - extract token value from XML token
NB. NB. ============================================================

NB. GetTokenValue =: 3 : 0

NB.   tok =. y

NB.   gt =. tok i. '>'

NB.   rest =. (gt+1) }. tok

NB.   lt =. rest i. '<'

NB.   dltb lt {. rest

NB. )


NB. NB. ============================================================
NB. NB. Helper - identifier info XML line
NB. NB. ============================================================

NB. IdentifierInfoLine =: 4 : 0
NB.   indent =. x
NB.   name =. y
NB.   kind =. KindOf name
NB.   index =. IndexOf name

NB.   (Spaces indent) , '<identifierInfo name="' , name , '" category="' , kind , '" index="' , (": index) , '" usage="used" />' , LF
NB. )

NB. DeclaredIdentifierInfoLine =: 4 : 0
NB.   indent =. x
NB.   name =. y
NB.   kind =. KindOf name
NB.   index =. IndexOf name

NB.   (Spaces indent) , '<identifierInfo name="' , name , '" category="' , kind , '" index="' , (": index) , '" usage="declared" />' , LF
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - classVarDec
NB. NB. ============================================================

NB. CompileClassVarDec =: 3 : 0

NB.   out =. 2 TagLine '<classVarDec>'

NB.   kind =. GetTokenValue CurrentToken ''
NB.   out =. out , 4 Consume ''

NB.   type =. GetTokenValue CurrentToken ''
NB.   out =. out , 4 Consume ''

NB.   name =. GetTokenValue CurrentToken ''
NB.   name Define (type ; kind)
NB.   out =. out , 4 DeclaredIdentifierInfoLine name
NB.   out =. out , 4 Consume ''

NB.   while. TokenIs ',' do.
NB.       out =. out , 4 Consume ''

NB.       name =. GetTokenValue CurrentToken ''
NB.       name Define (type ; kind)
NB.       out =. out , 4 DeclaredIdentifierInfoLine name
NB.       out =. out , 4 Consume ''
NB.   end.

NB.   out =. out , 4 Consume ''
NB.   out =. out , 2 TagLine '</classVarDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - varDec
NB. NB. ============================================================

NB. CompileVarDec =: 3 : 0

NB.   out =. y TagLine '<varDec>'

NB.   out =. out , (y + 2) Consume ''

NB.   type =. GetTokenValue CurrentToken ''
NB.   out =. out , (y + 2) Consume ''

NB.   name =. GetTokenValue CurrentToken ''
NB.   name Define (type ; 'var')
NB.   out =. out , (y + 2) DeclaredIdentifierInfoLine name
NB.   out =. out , (y + 2) Consume ''

NB.   while. TokenIs ',' do.
NB.       out =. out , (y + 2) Consume ''

NB.       name =. GetTokenValue CurrentToken ''
NB.       name Define (type ; 'var')
NB.       out =. out , (y + 2) DeclaredIdentifierInfoLine name
NB.       out =. out , (y + 2) Consume ''
NB.   end.

NB.   out =. out , (y + 2) Consume ''
NB.   out =. out , y TagLine '</varDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - parameterList
NB. NB. ============================================================

NB. CompileParameterList =: 3 : 0

NB.   out =. y TagLine '<parameterList>'

NB.   if. -. TokenIs ')' do.
NB.       type =. GetTokenValue CurrentToken ''
NB.       out =. out , (y + 2) Consume ''

NB.       name =. GetTokenValue CurrentToken ''
NB.       name Define (type ; 'argument')
NB.       out =. out , (y + 2) DeclaredIdentifierInfoLine name
NB.       out =. out , (y + 2) Consume ''

NB.       while. TokenIs ',' do.
NB.           out =. out , (y + 2) Consume ''

NB.           type =. GetTokenValue CurrentToken ''
NB.           out =. out , (y + 2) Consume ''

NB.           name =. GetTokenValue CurrentToken ''
NB.           name Define (type ; 'argument')
NB.           out =. out , (y + 2) DeclaredIdentifierInfoLine name
NB.           out =. out , (y + 2) Consume ''
NB.       end.
NB.   end.

NB.   out =. out , y TagLine '</parameterList>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - subroutine
NB. NB. ============================================================

NB. CompileSubroutine =: 3 : 0

NB.   StartSubroutine ''

NB.   out =. 2 TagLine '<subroutineDec>'

NB.   subroutineType =: GetTokenValue CurrentToken ''
NB.   out =. out , 4 Consume ''

NB.   out =. out , 4 Consume ''

NB.   subroutineName =: GetTokenValue CurrentToken ''
NB.   out =. out , 4 Consume ''

NB.   out =. out , 4 Consume ''

NB.   out =. out , CompileParameterList 4

NB.   out =. out , 4 Consume ''

NB.   out =. out , CompileSubroutineBody 4

NB.   out =. out , 2 TagLine '</subroutineDec>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - letStatement identifier use
NB. NB. ============================================================

NB. CompileLet =: 3 : 0

NB.   out =. y TagLine '<letStatement>'

NB.   out =. out , (y + 2) Consume ''

NB.   name =. GetTokenValue CurrentToken ''
NB.   out =. out , (y + 2) IdentifierInfoLine name
NB.   out =. out , (y + 2) Consume ''

NB.   if. TokenIs '[' do.
NB.     out =. out , (y + 2) Consume ''
NB.     out =. out , CompileExpression (y + 2)
NB.     out =. out , (y + 2) Consume ''
NB.   end.

NB.   out =. out , (y + 2) Consume ''
NB.   out =. out , CompileExpression (y + 2)
NB.   out =. out , (y + 2) Consume ''

NB.   out =. out , y TagLine '</letStatement>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - term identifier use
NB. NB. ============================================================

NB. CompileTerm =: 3 : 0

NB.   out =. y TagLine '<term>'

NB.   if. TokenIs '(' do.

NB.     out =. out , (y + 2) Consume ''
NB.     out =. out , CompileExpression (y + 2)
NB.     out =. out , (y + 2) Consume ''

NB.   elseif. (TokenIs '-') +. (TokenIs '~') do.

NB.     out =. out , (y + 2) Consume ''
NB.     out =. out , CompileTerm (y + 2)

NB.   elseif. IsIdentifier '' do.

NB.     name =. GetTokenValue CurrentToken ''

NB.     if. NextTokenIs '[' do.

NB.       out =. out , (y + 2) IdentifierInfoLine name
NB.       out =. out , (y + 2) Consume ''
NB.       out =. out , (y + 2) Consume ''
NB.       out =. out , CompileExpression (y + 2)
NB.       out =. out , (y + 2) Consume ''

NB.     elseif. (NextTokenIs '(') +. (NextTokenIs '.') do.

NB.       out =. out , CompileSubroutineCall (y + 2)

NB.     else.

NB.       out =. out , (y + 2) IdentifierInfoLine name
NB.       out =. out , (y + 2) Consume ''

NB.     end.

NB.   else.

NB.     out =. out , (y + 2) Consume ''

NB.   end.

NB.   out =. out , y TagLine '</term>'

NB.   out
NB. )

NB. NB. ============================================================
NB. NB. Stage 1 - subroutineCall identifiers
NB. NB. ============================================================

NB. CompileSubroutineCall =: 3 : 0

NB.   out =. ''

NB.   firstName =. GetTokenValue CurrentToken ''

NB.   if. NextTokenIs '.' do.

NB.     if. -. (KindOf firstName) -: 'none' do.
NB.       out =. out , y IdentifierInfoLine firstName
NB.     else.
NB.       out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="class" index="-1" usage="used" />' , LF
NB.     end.

NB.     out =. out , y Consume ''
NB.     out =. out , y Consume ''

NB.     secondName =. GetTokenValue CurrentToken ''
NB.     out =. out , (Spaces y) , '<identifierInfo name="' , secondName , '" category="subroutine" index="-1" usage="used" />' , LF
NB.     out =. out , y Consume ''

NB.   else.

NB.     out =. out , (Spaces y) , '<identifierInfo name="' , firstName , '" category="subroutine" index="-1" usage="used" />' , LF
NB.     out =. out , y Consume ''

NB.   end.

NB.   out =. out , y Consume ''
NB.   out =. out , CompileExpressionList y
NB.   out =. out , y Consume ''

NB.   out
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

NB. smoutput 'ex5 loaded.'