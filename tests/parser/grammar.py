from codeGen import *

# globals
outerClassName = 'defaultOuterClassName'

# scope definitions
a = ['allowcolon','nocolon']
allowColon = ['allowcolon']
noColon = ['nocolon']

b = ['allowin','noin']
allowIn = ['allowin']
noIn = ['noin']

g = ['allowexpr','noexpr']
allowExpr = ['allowexpr']
noExpr = ['noexpr']

t = ['global','class','interface','local']
gl = ['global']
cl = ['class']
interface = ['interface']
local = ['local']

w = ['abbrev','noshortif','full']
abbrev = ['abbrev']
noShortIf = ['noshortif']
full = ['full']

#{ Node : [ { scope : [''], children : [ { subnodes : [{name:'',scope:''}], syntax : ''|lambda, literal : boolean} ] } ] } 


### Lexical Structure ###

nn('ReservedIdentifier')
ac('','break',True)
ac('','case',True)
ac('','cast',True)
ac('','catch',True)
ac('','class',True)
ac('','continue',True)
ac('','debugger',True)
ac('','default',True)
ac('','delete',True)
ac('','do',True)
ac('','else',True)
ac('','enum',True)
ac('','extends',True)
ac('','false',True)
ac('','finally',True)
ac('','for',True)
ac('','function',True)
ac('','if',True)
ac('','in',True)
ac('','instanceof',True)
ac('','internal',True)
ac('','is',True)
ac('','new',True)
ac('','null',True)
ac('','private',True)
ac('','protected',True)
ac('','public',True)
ac('','return',True)
ac('','super',True)
ac('','switch',True)
ac('','this',True)
ac('','throw',True)
ac('','true',True)
ac('','try',True)
ac('','typeof',True)
ac('','var',True)
ac('','void',True)
ac('','while',True)
ac('','with',True)
ac('','wrap',True)

nn('ContextuallyReservedIdentifier')
ac('','const',True)
ac('','decimal',True)
ac('','double',True)
ac('','dynamic',True)
ac('','each',True)
ac('','eval',True)
ac('','final',True)
ac('','generator',True)
ac('','generic',True)
ac('','get',True)
ac('','has',True)
ac('','implements',True)
ac('','import',True)
ac('','int',True)
ac('','interface',True)
ac('','intrinsic',True)
ac('','let',True)
ac('','namespace',True)
ac('','native',True)
ac('','Number',True)
ac('','override',True)
ac('','package',True)
ac('','precision',True)
ac('','prototype',True)
ac('','rounding',True)
ac('','set',True)
ac('','standard',True)
ac('','static',True)
ac('','strict',True)
ac('','to',True)
ac('','type',True)
ac('','uint',True)
ac('','undefined',True)
ac('','unit',True)
ac('','use',True)
ac('','xml',True)
ac('','yield',True)

#TODO: Implement virtual ; behavior
nn('VirtualSemicolon')
ac('',';',True)

# Not fully fleshed out as per E262 7.6
nn('LexIdentifier') #renamed from Identifier in E262
def RandomTextIdentifier():
    chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_'
    str = ''
    for i in range(random.randint(3,15)):
        str += random.choice(chars)
    #can't start with a number
    try:
        while str[0] in '0123456789':
            str = str[1:]
    except IndexError:
        str = 'haha'
    return str     
ac('RandomTextIdentifier',RandomTextIdentifier,True) 

#TODO: Do I need to use E262 7.8.4 here or is this adequate?
nn('StringLiteral')
def RandomStringLiteral():
    chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_~!@#%^&*()-=+`{}\\|;:<>,./?'
    str = ''
    for i in range(random.randint(0,10)):
        str += random.choice(chars)
    #wrap in either " or '
    if random.randint(0,1):
        str = '"'+str+'"'
    else:
        str = "'"+str+"'"
    return str
ac('RandomStringLiteral',RandomStringLiteral,True)


# NuberLiteral
nn('NumberLiteral')
ac('DecimalLiteral')
ac('HexIntegerLiteral')

nn('DecimalLiteral')
ac([('DecimalIntegerLiteral'),('DecimalDigits'),('ExponentPart')],'%s.%s%s')
ac([('DecimalIntegerLiteral'),('DecimalDigits')],'%s.%s')
ac([('DecimalIntegerLiteral'),('ExponentPart')],'%s%s')
ac('DecimalIntegerLiteral')
ac([('DecimalDigits'),('ExponentPart')],'.%s%s')
ac(('DecimalDigits'),'.%s')
ac([('DecimalIntegerLiteral'),('ExponentPart')],'%s%s')
ac([('DecimalIntegerLiteral')])

nn('DecimalIntegerLiteral')
ac('zero','0',True)
ac([('NonZeroDigit'),('DecimalDigits')],'%s%s')
ac('NonZeroDigit')

nn('DecimalDigits')
ac('DecimalDigit')
ac([('DecimalDigits'),('DecimalDigit')],'%s%s')

nn('DecimalDigit')
ac('',lambda : str(random.randint(0,9)),True)

nn('NonZeroDigit')
ac('',lambda : str(random.randint(1,9)),True)

nn('ExponentPart')
ac([('ExponentIndicator'),('SignedInteger')],'%s%s')

nn('ExponentIndicator')
ac('',lambda : random.choice('eE'), True)

nn('SignedInteger')
ac('DecimalDigits')
ac('DecimalDigits', '+%s')
ac('DecimalDigits', '-%s')

nn('HexDigit')
ac('',lambda : random.choice('0123456789abcdefABCDEF'),True)

nn('HexIntegerLiteral')
ac('HexDigit','0x%s')
ac('HexDigit','0X%s')
ac([('HexIntegerLiteral'),('HexDigit')],'%s%s')
# End NumberLiteral

#RegularExpression
nn('RegularExpression')
ac('RegularExpressionLiteral')

#[see Ecma-262 section 7.8.5]
nn('RegularExpressionLiteral')
ac([('RegularExpressionBody'),('RegularExpressionFlags')],'/ %s / %s')

nn('RegularExpressionBody')
ac([('RegularExpressionFirstChar'),('RegularExpressionChars')])

nn('RegularExpressionChars')
ac('Empty')
ac([('RegularExpressionChars'),('RegularExpressionChar')])

nn('RegularExpressionFirstChar')
#NonTerminator but not * or \ or / 
ac('', lambda : replaceChars(getNodeStr('NonTerminator'),('*\\/'),'$'),True)
ac('BackslashSequence')

nn('RegularExpressionChar')
#NonTerminator but not \ or / 
ac('', lambda : replaceChars(getNodeStr('NonTerminator'),'\\/','$'),True)
ac('BackslashSequence')

nn('BackslashSequence')
ac('NonTerminator','\ %s')

nn('NonTerminator')
#SourceCharacter but not LineTerminator
#TODO: SourceCharacterr does not have any LineTerminator in it, so don't bother w/ replace
# getting some strange infinite loop w/ replaceChars code below
#ac('',lambda : replaceChars(getNodeStr('SourceCharacter'),getNodeStr('LineTerminator'),'a'),True)
ac('SourceCharacter')

nn('RegularExpressionFlags')
ac('Empty')
ac([('RegularExpressionFlags'),('IdentifierPart')])

nn('IdentifierPart')
ac('', lambda : random.choice('bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$'),True)

#Add LineTerminator since that isn't specified otherwise
nn('LineTerminator')
ac('linefeed','\n',True)
ac('carriageReturn','\r',True)
#line separator
#paragraph separator


### XML ###
#[see Ecma-357 section 8.3]

#have to implement SourceCharacter for XML
nn('SourceCharacters')
ac([('SourceCharacter'),('SourceCharacters')])
ac('SourceCharacter')

nn('SourceCharacter') #changed from spec - no unicode
ac('',lambda : random.choice('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_~!@#%^&*()-=+`/{}\\|;:<>,.?'),True)

nn('XMLMarkup') 
ac('XMLComment')
ac('XMLCDATA')
ac('XMLPI')

nn('XMLTagCharacters')
# SourceCharacters but no embedded XMLTagPunctuator 
#or left-curly { or quote ' or double-quote " or forward-slash / or XMLWhitespaceCharacter 
ac('',lambda : replaceChars(getNodeStr('SourceCharacters'),'{/','*'),True) #replace w/ a char to avoid empty string

nn('XMLWhitespaceCharacter')
ac('',' ',True) #<SP>
ac('','\t',True) #<TAB>
ac('','\r',True) #<CR>
ac('','\n',True) #<LF>

nn('XMLWhitespace')
ac('XMLWhitespaceCharacter')
ac([('XMLWhitespace'),('XMLWhitespaceCharacter')])
 
nn('XMLText')
# SourceCharacters but no embedded left-curly { or less-than < 
ac('',lambda : replaceChars(getNodeStr('SourceCharacters'),'{<','*'),True)
 
#TODO: Proper Unicode chars
nn('UnicodeLetter')
ac('',lambda : random.choice('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'),True)

nn('UnicodeDigit')
ac('',lambda : random.choice('0123456789'),True)

nn('XMLName')
ac('XMLNameStart')
ac([('XMLName'),('XMLNamePart')])
 
nn('XMLNameStart')
ac('UnicodeLetter')
ac('underscore','_',True)
ac('colon',':',True)
 
nn('XMLNamePart')
ac('UnicodeLetter')
ac('UnicodeDigit')
ac('period','.',True) 
ac('hyphen','-',True) 
ac('underscore','_',True)
ac('colon',':',True) 
 
nn('XMLComment')
ac('emptyComments','<!--  -->',True)
ac('XMLCommentCharacters','<!-- %s -->')
 
nn('XMLCommentCharacters')
# SourceCharacters but no embedded sequence -- 
ac('',lambda : getNodeStr('SourceCharacters').replace('--','$'),True)
 
nn('XMLCDATA')
ac('','<![CDATA[  ]]>',True)
ac('XMLCDATACharacters','<![CDATA[ %s ]]>')
 
nn('XMLCDATACharacters')
#SourceCharacters but no embedded sequence ]]> 
ac('',lambda : getNodeStr('SourceCharacters').replace(']]>','$'),True)
 
nn('XMLPI')
ac('','<?  ?>',True)
ac('XMLPICharacters','<? %s ?>')
 
nn('XMLPICharacters')
#SourceCharacters but no embedded sequence ?>
ac('',lambda : getNodeStr('SourceCharacters').replace('?>','$'),True)
 
nn('XMLAttributeValue')
#" XMLDoubleStringCharactersopt " 
ac('XMLDoubleStringCharacters','" %s "')
#' XMLSingleStringCharactersopt ' 
ac('XMLSingleStringCharacters',"' %s '")
 
nn('XMLDoubleStringCharacters')
#SourceCharacters but no embedded double-quote " 
ac('',lambda : getNodeStr('SourceCharacters').replace('"','$'),True)
 
nn('XMLSingleStringCharacters')
#SourceCharacters but no embedded single-quote ' 
ac('',lambda : getNodeStr('SourceCharacters').replace("'",'$'),True)

## End XML

### Syntactic Structure ###

## Expressions ##
nn('Identifier')
ac('LexIdentifier') #Note that this is changed from grammar.xls from bold Identifier to LexIdentifier
ac('ContextuallyReservedIdentifier')

nn('Qualifier')
ac('','*',True)
ac('Identifier')
ac('ReservedNamespace')

nn('ReservedNamespace')
ac('',lambda : random.choice('internal intrinsic private protected public'.split()),True)

nn('QualifiedNameIdentifier')
ac('Identifier')
ac('ReservedIdentifier')
ac('StringLiteral')
ac('NumberLiteral')
ac('Brackets')
ac('OverloadedOperator')

nn('QualifiedName')
ac('Identifier')
ac([('Qualifier'),('QualifiedNameIdentifier')],'%s :: %s')
ac([('ParenListExpression'),('QualifiedNameIdentifier')],'%s :: %s')
ac([('QualifiedName'),('QualifiedNameIdentifier')],'%s :: %s')

nn('AttributeName')
ac('Brackets','@ %s')
ac('QualifiedName','@ %s')

nn('PropertyName')
ac('AttributeName')
ac('QualifiedName')

nn('PrimaryName')
ac([('Path'),('QualifiedName')],'%s . %s')
ac('QualifiedName')

nn('Path')
ac('Identifier')
ac([('Path'),('Identifier')],'%s . %s')

nn('ParenExpression')
ac(('AssignmentExpression',allowColon+allowIn),'( %s )')

nn('ParenListExpression')
ac(('ListExpression',allowColon+allowIn), '( %s )')

nn('FunctionExpression',a+b)
ac([('Identifier'),('FunctionSignature'),('FunctionExpressionBody', a+b)],'function %s %s %s')
ac([('FunctionSignature'),('FunctionExpressionBody', a+b)],'function %s %s')

nn('FunctionExpressionBody', a+b)
ac(('Block',local))
ac(('AssignmentExpression', a+b))

nn('ObjectLiteral',noColon)
ac('FieldList','{ %s }')

nn('ObjectLiteral',allowColon)
ac('FieldList','{ %s }')
ac([('FieldList'),('TypeExpression')],'{ %s } : %s')

nn('FieldList')
ac('Empty')
ac('LiteralField')
ac([('LiteralField'),('FieldList')],'%s , %s')

nn('LiteralField')
ac([('FieldKind'),('FieldName'),('AssignmentExpression', allowColon+allowIn)],'%s %s : %s')
ac([('FieldName'),('FunctionSignature'),('FunctionExpressionBody', allowColon+allowIn)],'get %s %s %s')
ac([('FieldName'),('FunctionSignature'),('FunctionExpressionBody', allowColon+allowIn)],'set %s %s %s')

nn('FieldKind')
ac('Empty')
ac('','const',True)

nn('FieldName')
ac('QualifiedName')
ac('StringLiteral')
ac('NumberLiteral')
ac('ReservedIdentifier')

nn('ArrayLiteral',noColon)
ac('Elements','[ %s ]')

nn('ArrayLiteral',allowColon)
ac('Elements','[ %s ]')
ac([('Elements'),('TypeExpression')], '[ %s ] : %s')

nn('Elements')
ac('ElementList')
ac('ElementComprehension')

nn('ElementList')
ac('Empty')
ac('LiteralElement')
ac('ElementList',', %s')
ac([('LiteralElement'),('ElementList')],'%s, %s')

nn('LiteralElement')
ac(('AssignmentExpression', allowColon+allowIn))

nn('ElementComprehension')
ac([('LiteralElement'),('ForInExpressionList'),('OptionalIfCondition')])

nn('ForInExpressionList')
ac('ForInExpression')
ac([('ForInExpressionList'),('ForInExpression')])

nn('ForInExpression')
ac([('ForInBinding'),('ListExpression',allowColon+allowIn)],'for ( %s in %s )')
ac([('ForInBinding'),('ListExpression',allowColon+allowIn)],'for each ( %s in %s )')

nn('OptionalIfCondition')
ac('Empty')
ac('ParenListExpression','if %s')


nn('XMLInitialiser')
ac('XMLMarkup')
ac('XMLElement')
ac('XMLElementContent','< > %s </ >')

nn('XMLElementContent')
ac([('ListExpression',allowColon+allowIn),('XMLElementContent')],'{ %s } %s')
ac([('XMLMarkup'),('XMLElementContent')])
ac([('XMLText'),('XMLElementContent')])
ac([('XMLElement'),('XMLElementContent')])
ac('Empty')

nn('XMLElement')
ac([('XMLTagContent'),('XMLWhitespace')],'< %s %s />')
ac('XMLTagContent','< %s />')
ac([('XMLTagContent'),('XMLWhitespace'),('XMLElementContent'),('XMLTagName'),('XMLWhitespace')],
    '< %s %s > %s </ %s %s >')
ac([('XMLTagContent'),('XMLElementContent'),('XMLTagName'),('XMLWhitespace')],
    '< %s > %s </ %s %s >')
ac([('XMLTagContent'),('XMLWhitespace'),('XMLElementContent'),('XMLTagName')],
    '< %s %s > %s </ %s >')
ac([('XMLTagContent'),('XMLElementContent'),('XMLTagName')],'< %s > %s </ %s >')


nn('XMLTagContent')
ac([('XMLTagName'),('XMLAttributes')])

nn('XMLTagName')
ac(('ListExpression',allowColon+allowIn),'{ %s }')
ac('XMLName')

nn('XMLAttributes')
ac([('XMLWhitespace'),('ListExpression',allowColon+allowIn)],'%s { %s }')
ac([('XMLAttribute'),('XMLAttributes')])
ac('Empty')

nn('XMLAttribute')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s %s = %s { %s }')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s = %s { %s }')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s %s = { %s }')
ac([('XMLWhitespace'),('XMLName'),('ListExpression',allowColon+allowIn)],'%s %s = { %s }')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLWhitespace'),('XMLAttributeValue')],
    '%s %s %s = %s %s')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLAttributeValue')],'%s %s = %s %s')
ac([('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLAttributeValue')],'%s %s %s = %s')
ac([('XMLWhitespace'),('XMLName'),('XMLAttributeValue')],'%s %s = %s')

nn('PrimaryExpression', a+b)
ac('null','null',True)
ac('true','true',True)
ac('false','false',True)
ac('NumberLiteral')
ac('StringLiteral')
ac('RegularExpression')
ac('XMLInitialiser')
ac(('ArrayLiteral',a))
ac(('ObjectLiteral',a))
ac(('FunctionExpression', a+b))
ac('ThisExpression')
ac('ParenListExpression')
ac('PrimaryName')

nn('ThisExpression')
ac('this','this',True)
ac('this function', 'this function',True)
ac('this generator','this generator',True)

nn('SuperExpression')
ac('super','super',True)
ac('ParenExpression','super %s')

nn('Arguments')
ac('','( )',True)
ac('ArgumentList','( %s )')

nn('ArgumentList')
ac(('AssignmentExpression', allowColon+allowIn))
ac([('ArgumentList'),('AssignmentExpression', allowColon+allowIn)],'%s, %s')

nn('PropertyOperator')
ac('ReservedIdentifier','.%s')
ac('PropertyName','.%s')
ac('QualifiedName','..%s')
ac('ParenListExpression','.%s')
ac([('ParenListExpression'),('QualifiedNameIdentifier')],'.%s::%s')
ac('Brackets')
ac('TypeApplication')

nn('Brackets')
ac(('ListExpression', allowColon+allowIn),'[ %s ]')
ac('SliceExpression', '[ %s ]')
	
nn('TypeApplication')
ac('TypeExpressionList','.< %s >')
	
nn('SliceExpression')
ac([('OptionalExpression'),('OptionalExpression')],'%s : %s')
ac([('OptionalExpression'),('OptionalExpression'),('OptionalExpression')],'%s : %s :%s')
	
nn('OptionalExpression')
ac(('ListExpression', allowColon+allowIn))
ac('Empty')
	
nn('MemberExpression',a+b)	
ac(('PrimaryExpression', a+b))
ac([('MemberExpression',a+b),('Arguments')],'new %s %s')
ac([('SuperExpression'),('PropertyOperator')])
ac([('MemberExpression',a+b),('PropertyOperator')])
	
nn('CallExpression', a+b)	
ac([('MemberExpression',a+b),('Arguments')])
ac([('CallExpression',a+b),('Arguments')])
ac([('CallExpression',a+b),('PropertyOperator')])

nn('NewExpression',a+b)
ac(('MemberExpression',a+b))
ac(('NewExpression',a+b),'new %s')

nn('LeftHandSideExpression',a+b)
ac(('NewExpression',a+b))
ac(('CallExpression',a+b))
	
nn('UnaryTypeExpression',a+b)
ac(('LeftHandSideExpression',a+b))
ac('TypeExpression','type %s')
	
nn('PostfixExpression',a+b)	
ac(('UnaryTypeExpression',a+b))
ac(('LeftHandSideExpression', a+b),'%s ++')    #LeftHandSideExpressiona, b  [no line break]  ++
ac(('LeftHandSideExpression', a+b),'%s --')    #LeftHandSideExpressiona, b  [no line break]  --
	
nn('UnaryExpression', a+b)
ac(('PostfixExpression',a+b))
ac(('PostfixExpression', a+b),'delete %s')
ac(('UnaryExpression', a+b),'void %s')
ac(('UnaryExpression', a+b),'typeof %s')
ac(('PostfixExpression', a+b),'++ %s')
ac(('PostfixExpression', a+b),'-- %s')
ac(('UnaryExpression', a+b),'+ %s')
ac(('UnaryExpression', a+b),'- %s')
ac(('UnaryExpression', a+b),'~ %s')
ac(('UnaryExpression', a+b),'! %s')

	
nn('MultiplicativeExpression',a+b)
ac(('UnaryExpression', a+b))
ac([('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s * %s')
ac([('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s / %s')
ac([('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s %% %s') #Note double %% to return % after formatting

nn('AdditiveExpression',a+b)
ac(('MultiplicativeExpression', a+b))
ac([('AdditiveExpression', a+b),('MultiplicativeExpression', a+b)],'%s + %s')
ac([('AdditiveExpression', a+b),('MultiplicativeExpression', a+b)],'%s - %s')

nn('ShiftExpression',a+b)
ac(('AdditiveExpression', a+b))
ac([('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s << %s')
ac([('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s >> %s')
ac([('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s >>> %s')

nn('RelationalExpression',a+allowIn)
ac(('ShiftExpression', b))
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s < %s')
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s > %s')
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s <= %s')
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s >= %s')
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s in %s')
ac([('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s instanceof %s')
ac([('RelationalExpression', a+allowIn),('TypeExpression')],'%s cast %s')
ac([('RelationalExpression', a+allowIn),('TypeExpression')],'%s is %s')
ac([('RelationalExpression', a+allowIn),('TypeExpression')],'%s wrap %s')

nn('RelationalExpression',a+noIn)
ac(('ShiftExpression', a+b))
ac([('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s < %s')
ac([('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s > %s')
ac([('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s <= %s')
ac([('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s >= %s')
ac([('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s instanceof %s')
ac([('RelationalExpression', a+noIn),('TypeExpression')],'%s cast %s')
ac([('RelationalExpression', a+noIn),('TypeExpression')],'%s is %s')
ac([('RelationalExpression', a+noIn),('TypeExpression')],'%s wrap %s')

nn('EqualityExpression',a+b)
ac(('RelationalExpression', a+b))
ac([('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s == %s')
ac([('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s != %s')
ac([('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s === %s')
ac([('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s !== %s')

nn('BitwiseAndExpression',a+b)
ac(('EqualityExpression',a+b))
ac([('BitwiseAndExpression', a+b),('EqualityExpression', a+b)],'%s & %s')

nn('BitwiseXorExpression',a+b)
ac(('BitwiseAndExpression',a+b))
ac([('BitwiseXorExpression', a+b),('BitwiseAndExpression', a+b)],'%s ^ %s')	

nn('BitwiseOrExpression',a+b)
ac(('BitwiseXorExpression',a+b))
ac([('BitwiseOrExpression', a+b),('BitwiseXorExpression', a+b)],'%s | %s')		

nn('LogicalAndExpression',a+b)
ac(('BitwiseOrExpression',a+b))
ac([('LogicalAndExpression', a+b),('BitwiseOrExpression', a+b)],'%s && %s')		

nn('LogicalOrExpression',a+b)
ac(('LogicalAndExpression',a+b))
ac([('LogicalOrExpression', a+b),('LogicalOrExpression', a+b)],'%s || %s')		

nn('ConditionalExpression',a+b)
ac(('LetExpression',a+b))
ac(('YieldExpression',a+b))
ac(('LogicalOrExpression',a+b))
ac([('LogicalOrExpression', a+b),('AssignmentExpression',noColon+b),('AssignmentExpression', a+b)],'%s ? %s : %s')		

nn('NonAssignmentExpression',a+b)
ac(('LetExpression',a+b))
ac(('YieldExpression',a+b))
ac(('LogicalOrExpression',a+b))
ac([('LogicalOrExpression', a+b),('NonAssignmentExpression',noColon+b),('NonAssignmentExpression', a+b)],'%s ? %s : %s')	

nn('LetExpression',a+b)
ac([('LetBindingList'),('AssignmentExpression', a+b)],'let ( %s ) %s')
	
nn('LetBindingList')
ac('Empty')
ac('NonemptyLetBindingList')

nn('NonemptyLetBindingList')
ac(('VariableBinding',allowIn))
ac([('VariableBinding',allowIn),('NonemptyLetBindingList')],'%s, %s')
	
nn('YieldExpression',a+b)
ac('yield','yield',True)
ac(('AssignmentExpression',a+b),'yield %s') #yield  [no line break]  AssignmentExpressiona, b
	
nn('AssignmentExpression',a+b)	
ac(('ConditionalExpression',a+b))
ac([('Pattern',a+b+allowExpr),('AssignmentExpression',a+b)],'%s = %s')
ac([('SimplePattern',a+b+allowExpr),('CompoundAssignmentOperator'),('AssignmentExpression',a+b)])
	
nn('CompoundAssignmentOperator')
ac('',lambda : random.choice('*= /= %= += -= <<= >>= >>>= &= ^= |= &&= ||='.split(' ')),True)

nn('ListExpression',a+b)	
ac(('AssignmentExpression',a+b))
ac([('ListExpression',a+b),('AssignmentExpression',a+b)],'%s, %s')

## Patterns ##
nn('Pattern', a+b+g)	
ac(('SimplePattern',a+b+g))
ac(('ObjectPattern', a+b+g))
ac(('ArrayPattern',g))
	
nn('SimplePattern',a+b+noExpr)
ac('Identifier')
	
nn('SimplePattern',a+b+allowExpr)
ac(('LeftHandSideExpression',a+b))
	
nn('ObjectPattern',g)	
ac(('FieldListPattern',g),'{ %s }')
	
nn('FieldListPattern',g)
ac('Empty')
ac(('FieldPattern',g))
ac([('FieldListPattern',g),('FieldPattern',g)],'%s, %s')
	
nn('FieldPattern',g)
ac('FieldName')
ac([('FieldName'),('Pattern',allowColon+allowIn+g)],'%s : %s')
	
nn('ArrayPattern',g)
ac(('ElementListPattern',g),'[ %s ]')
	
nn('ElementListPattern',g)
ac('Empty')
ac(('ElementPattern',g))
ac(('ElementListPattern',g),', %s')
ac([('ElementPattern',g),('ElementListPattern',g)],'%s, %s')

nn('ElementPattern',g)
ac(('Pattern',allowColon+allowIn+g))
	
nn('TypedIdentifier')
ac(('SimplePattern',allowColon+allowIn+noExpr))
ac([('SimplePattern',allowColon+allowIn+noExpr),('TypeAnnotation')],'%s : %s')
	
nn('TypedPattern',b)	
ac(('Pattern',allowColon+b+noExpr))
ac([('Pattern',allowColon+b+noExpr),('TypeAnnotation')],'%s : %s')
	
nn('TypeAnnotation')
ac('TypeExpression')
ac('TypeExpression','wrap %s')

## Type Expressions ##
nn('TypeExpression')
ac('NullableTypeExpression')
ac('NullableTypeExpression','like %s')

nn('NullableTypeExpression')
ac('BasicTypeExpression')
ac('BasicTypeExpression','%s ?')
ac('BasicTypeExpression','%s !')

nn('BasicTypeExpression')
ac('*','*',True)
ac('null','null',True)
ac('undefined','undefined',True)
ac('FunctionType')
ac('UnionType')
ac('RecordType')
ac('ArrayType')
ac('PrimaryName')

nn('FunctionType')
ac('FunctionSignatureType','function %s')

nn('FunctionSignatureType')
ac([('TypeParameters'),('ParametersType'),('ResultType')],'%s ( %s ) %s')
ac([('TypeParameters'),('PrimaryName'),('ResultType')],'%s ( this : %s ) %s')
ac([('TypeParameters'),('PrimaryName'),('NonemptyParametersType'),('ResultType')],'%s ( this: %s, %s ) %s')

nn('ParametersType')
ac('Empty')
ac('NonemptyParametersType')

nn('NonemptyParametersType')
ac('ParameterInitType')
ac([('ParameterInitType'),('NonemptyParametersType')],'%s, %s')
ac('RestParameterType')

nn('ParameterInitType')
ac('ParameterType')
ac('ParameterType','%s =')

nn('ParameterType')
ac('TypeExpression')

nn('RestParameterType')
ac('','...', True)
ac('ParameterType','... %s')

nn('UnionType')
ac('TypeExpressionList','( %s )')

nn('RecordType')
ac('FieldTypeList','{ %s }')

nn('FieldTypeList')
ac('Empty')
ac('NonemptyFieldTypeList')

nn('NonemptyFieldTypeList')
ac('FieldType')
ac([('FieldType'),('NonemptyFieldTypeList')],'%s, %s')

nn('FieldType')
ac([('FieldName'),('TypeExpression')],'%s : %s')

nn('ArrayType')
ac('ElementTypeList','[ %s ]')

nn('ElementTypeList')
ac('Empty')
ac('TypeExpression')
ac('ElementTypeList',', %s')
ac([('TypeExpression'),('ElementTypeList')],'%s, %s')

nn('TypeExpressionList')
ac('TypeExpression')
ac([('TypeExpressionList'),('TypeExpression')],'%s, %s')

### Statements ###
nn('Statement',t+w)
ac(('BlockStatement',t))
ac([('BreakStatement'),('Semicolon',w)])
ac([('ContinueStatement'),('Semicolon',w)])
ac([('DefaultXMLNamespaceStatement'),('Semicolon',w)])
ac([('DoStatement'),('Semicolon',w)])
ac([('ExpressionStatement'),('Semicolon',w)])
ac(('ForStatement',w))
ac(('IfStatement',w))
ac(('LabeledStatement',w))
ac(('LetStatement',w))
ac([('ReturnStatement'),('Semicolon',w)])
ac('SwitchStatement')
ac('SwitchTypeStatement')
ac([('ThrowStatement'),('Semicolon',w)])
ac('TryStatement')
ac(('WhileStatement',w))
ac(('WithStatement',w))

nn('Substatement',w)
ac('EmptyStatement')
ac(('Statement',local+w))

nn('Semicolon',abbrev) 
ac(';',';',True)
ac('VirtualSemicolon')
ac('Empty')

nn('Semicolon',noShortIf)
ac('',';',True)
ac('VirtualSemicolon')
ac('Empty')

nn('Semicolon',full)
ac('',';',True)
ac('VirtualSemicolon')

nn('EmptyStatement')
ac('',';',True)

nn('ExpressionStatement')
ac(('ListExpression',allowColon+allowIn))
##TODO: whats lookahead?
##[lookahead !{ function, let, { }] ListExpressionallowColon, allowIn
	
nn('BlockStatement',t)
ac(('Block',t))
	
nn('LabeledStatement',w)	
ac([('Identifier'),('Substatement',w)])
	
nn('IfStatement',abbrev)
ac([('ParenListExpression'),('Substatement',abbrev)],'if %s %s')
ac([('ParenListExpression'),('Substatement',noShortIf),('Substatement',abbrev)],'if %s %s else %s')

nn('IfStatement',full)
ac([('ParenListExpression'),('Substatement',full)],'if %s %s')
ac([('ParenListExpression'),('Substatement',noShortIf),('Substatement',full)],'if %s %s else %s')

nn('IfStatement',noShortIf)
ac([('ParenListExpression'),('Substatement',noShortIf),('Substatement',noShortIf)],'if %s %s else %s')

nn('WithStatement',w)
ac([('TypedExpression'),('Substatement',w)],'with %s %s')

nn('TypedExpression')
ac('ParenListExpression')
ac([('ParenListExpression'),('TypeExpression')],'%s : %s')

nn('SwitchStatement')
ac([('ParenListExpression'),('CaseElements')],'switch %s { %s }')

nn('CaseElements')
ac('Empty')
ac('CaseLabel')
ac([('CaseLabel'),('CaseElementsPrefix'),('CaseLabel')])
ac([('CaseLabel'),('CaseElementsPrefix'),('Directive',local+abbrev)])

nn('CaseElementsPrefix')
ac('Empty')
ac([('CaseElementsPrefix'),('CaseLabel')])
ac([('CaseElementsPrefix'),('Directive',local+abbrev)])

nn('CaseLabel')
ac(('ListExpression',allowColon+allowIn),'case %s :')
ac('','default :',True)

nn('SwitchTypeStatement')
ac([('TypedExpression'),('TypeCaseElements')],'switch type %s { %s }')

nn('TypeCaseElements')
ac('TypeCaseElement')
ac([('TypeCaseElements'),('TypeCaseElement')])

nn('TypeCaseElement')
ac([('TypedPattern',allowColon+allowIn) ,('Block',local)],'case ( %s ) %s')

nn('DoStatement')
ac([('Substatement',abbrev),('ParenListExpression')],'do %s while %s')

nn('WhileStatement',w)
ac([('ParenListExpression'),('Substatement',w)],'while %s %s')

nn('ForStatement',w)
ac([('ForInitialiser'),('OptionalExpression'),('OptionalExpression'),('Substatement',w)],'for ( %s; %s; %s) %s')
ac([('ForInBinding'),('ListExpression',allowColon+allowIn),('Substatement',w)],'for ( %s in %s) %s')
ac([('ForInBinding'),('ListExpression',allowColon+allowIn),('Substatement',w)],'for each ( %s in %s) %s')

nn('ForInitialiser')
ac('Empty')
ac(('ListExpression',allowColon+noIn))
ac(('VariableDefinition',noIn))

nn('ForInBinding')
ac(('Pattern',allowColon+noIn+allowExpr))
ac([('VariableDefinitionKind'),('VariableBinding',noIn)])

nn('LetStatement',w)
ac([('LetBindingList'),('Block',local)],'let ( %s ) %s')

nn('ContinueStatement')
ac('','continue',True)
ac('Identifier','continue %s')

nn('BreakStatement')
ac('','break',True)
ac('Identifier','break %s')

nn('ReturnStatement')
ac('','return',True)
ac(('ListExpression',allowColon+allowIn),'return %s')

nn('ThrowStatement')
ac(('ListExpression',allowColon+allowIn),'throw %s')

nn('TryStatement')
ac([('Block',local),('CatchClauses')],'try %s %s')
ac([('Block',local),('CatchClauses'),('Block',local)],'try %s %s finally %s')
ac([('Block',local),('Block',local)],'try %s finally %s')

nn('CatchClauses')
ac('CatchClause')
ac([('CatchClauses'),('CatchClause')])

nn('CatchClause')
ac([('Parameter'),('Block',local)],'catch ( %s ) %s')

nn('DefaultXMLNamespaceStatement')
ac(('NonAssignmentExpression',allowColon+allowIn),'default xml namespace = %s')

## DIRECTIVES ##    

#t = { global, class, interface, local }

nn('Directives',t)
#ac('Empty')
ac([('DirectivesPrefix',t),('Directive',t+abbrev)])

nn('DirectivesPrefix',t)
#ac('Empty')
ac('Pragmas')
ac([('DirectivesPrefix',t),('Directive',t+full)])

nn('Directive',t+w)
#ac('EmptyStatement')
#ac(('Statement',t+w))
ac(('AnnotatableDirective',t+w))

nn('AnnotatableDirective',gl+w)
ac([('Attribute',gl),('AnnotatableDirective',gl+w)])
ac([('VariableDefinition',allowIn+w),('Semicolon',w)])
ac(('FunctionDefinition',gl+w))
ac('ClassDefinition')
ac('InterfaceDefinition')
ac([('NamespaceDefinition'),('Semicolon',w)])
ac([('TypeDefinition'),('Semicolon',w)])
ac('PackageDefinition')
ac('UnitDefinition')

nn('AnnotatableDirective',cl+w)
ac([('Attribute',gl),('AnnotatableDirective',cl+w)],'%s %s',nolb=[1])
ac(('VariableDefinition',allowIn+w))
ac(('FunctionDefinition',cl+w))
ac(('NamespaceDefinition',w))
ac(('TypeDefinition',w))

nn('AnnotatableDirective',interface+w)
ac([('Attribute',interface),('AnnotatableDirective',interface+w)],'%s %s',nolb=[1])
ac(('FunctionDeclaration',w))
ac(('TypeDefinition',w))

nn('AnnotatableDirective',local+w)
ac(('VariableDefinition',allowIn+w))
ac(('FunctionDefinition',local+w))
ac(('NamespaceDefinition',w))
ac(('TypeDefinition',w))

nn('Attribute',gl)
ac(('NamespaceAttribute',gl))
ac('','dynamic',True)
ac('','final',True)
ac('','native',True)
ac(('AssignmentExpression',allowColon+allowIn),'[ %s ]')

nn('Attribute',cl)
ac(('NamespaceAttribute',cl))
ac('','final',True)
ac('','native',True)
ac('','override',True)
ac('','prototype',True)
ac('','static',True)
ac(('AssignmentExpression',allowColon+allowIn),'[ %s ]')

nn('Attribute',interface)
ac('NamespaceAttribute')

nn('Attribute',local)
ac('NamespaceAttribute')

nn('NamespaceAttribute',gl)
ac('','public',True)
ac('','internal',True)
ac('','intrinsic',True)
ac('PrimaryName')

nn('NamespaceAttribute',cl)
ac('ReservedNamespace')
ac('PrimaryName')

## DEFINITIONS ##

nn('VariableDefinition',b+w)
ac([('VariableDefinitionKind'),('VariableBindingList',b)])

nn('VariableDefinitionKind')
ac('','const',True)
ac('','let',True)
ac('','let const',True)
ac('','var',True)

nn('VariableBindingList',b)
ac(('VariableBinding',b))
ac([('VariableBindingList',b),('VariableBinding',b)])

nn('VariableBinding',b)
ac('TypedIdentifier')
ac([('TypedPattern',b),('VariableInitialisation',b)])

nn('VariableInitialisation',b)
ac(('AssignmentExpression',allowColon+b),'= %s')

nn('FunctionDeclaration')
ac([('FunctionName'),('FunctionSignature')],'function %s %s')

nn('FunctionDefinition',cl+w)
#function  Identifier  [identifier == outer classname]  ConstructorSignature  Blocklocal
def fd():
    #print '********outerClassName: '+outerClassName
    return 'function %s %s %s' % (outerClassName, getNodeStr('ConstructorSignature'),getNodeStr('Block',local))
#ac('',lambda : 'function %s %s %s' % 
#    (outerClassName, getNodeStr('ConstructorSignature'),getNodeStr('Block',local)),True)
ac('',fd,True)
#DEBUG: ac([('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'function %s %s %s')


nn('FunctionDefinition',t+w)
ac([('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'function %s %s %s')
ac([('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'let function %s %s %s')
ac([('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'const function %s %s %s')

nn('FunctionName')
ac('Identifier')
ac('OverloadedOperator')
ac('Identifier','get %s')
ac('Identifier','set %s')

nn('OverloadedOperator')
ac('',lambda : random.choice('+ - ~ * / % < > <= >= == << >> >>> & | === != !=='.split()),True)

nn('FunctionSignature')
ac([('TypeParameters'),('Parameters'),('ResultType')],'%s ( %s ) %s')
ac([('TypeParameters'),('PrimaryName'),('ResultType')],'%s ( this : %s ) %s')
ac([('TypeParameters'),('PrimaryName'),('NonemptyParameters'),('ResultType')],'%s ( this : %s, %s ) %s')

nn('TypeParameters')
ac('Empty')
ac('TypeParameterList','.< %s >')  

nn('TypeParameterList')
ac('Identifier')
ac([('Identifier'),('TypeParameterList')],'%s, %s')

nn('Parameters')
ac('Empty')
ac('NonemptyParameters')

nn('NonemptyParameters')
ac('ParameterInit')
ac([('ParameterInit'),('NonemptyParameters')],'%s, %s')
ac('RestParameter')

nn('ParameterInit')
ac('Parameter')
ac([('Parameter'),('NonAssignmentExpression',allowIn)],'%s = %s')

nn('Parameter')
ac([('ParameterKind'),('TypedPattern',allowIn)])

nn('ParameterKind')
ac('Empty')
ac('','const',True)

nn('RestParameter')
ac('','...',True)
ac('Parameter','... %s')

nn('ResultType')
ac('Empty')
ac('',':  void',True)
ac('TypeAnnotation',': %s')

nn('ConstructorSignature')
ac([('TypeParameters'),('Parameters')],'%s ( %s )')
ac([('TypeParameters'),('Parameters'),('ConstructorInitialiser')],'%s ( %s ) : %s')

nn('ConstructorInitialiser')
ac('InitialiserList')
ac([('InitialiserList'),('SuperInitialiser')])
ac('SuperInitialiser')

nn('InitialiserList')
ac('Initialiser')
ac([('InitialiserList'),('Initialiser')],'%s , %s')

nn('Initialiser')
ac([('Pattern',allowIn+allowExpr),('VariableInitialisation',allowIn)])

nn('SuperInitialiser')
ac('Arguments','super %s')

nn('FunctionBody',a+b+w)
ac(('Block',local))
ac([('AssignmentExpression',a+b),('Semicolon',w)])

nn('ClassDefinition')
#The class identifier must be saved so that it can be reused for the constructor function in FunctionDefinition, class+w
def classDefinition():
    global outerClassName
    outerClassName = getNodeStr('Identifier')
    #print 'outerClassName: '+outerClassName
    return 'class %s %s %s %s' % (outerClassName,getNodeStr('TypeSignature'),getNodeStr('ClassInheritance'),getNodeStr('ClassBody'))
ac('',classDefinition,True)
#ac([('Identifier'),('TypeSignature'),('ClassInheritance'),('ClassBody')],'class %s %s %s %s')

nn('TypeSignature')
ac('TypeParameters')
ac('TypeParameters','%s !')

nn('ClassInheritance')
ac('Empty')
ac('TypeReference','extends %s')
ac('TypeReferenceList','implements %s')
ac([('TypeReference'),('TypeReferenceList')],'extends %s implements %s')

nn('TypeReferenceList')
ac('TypeReference')
ac([('TypeReference'),('TypeReferenceList')],'%s , %s')

nn('TypeReference')
ac('PrimaryName')
ac([('PrimaryName'),('TypeApplication')])

nn('ClassBody')
ac(('Block',cl))

nn('InterfaceDefinition')
ac([('Identifier'),('TypeSignature'),('InterfaceInheritance'),('InterfaceBody')],'interface  %s %s %s %s')

nn('InterfaceInheritance')
ac('Empty')
ac('TypeReferenceList','extends %s')

nn('InterfaceBody')
ac(('Block',interface))

nn('NamespaceDefinition')
ac([('Identifier'),('NamespaceInitialisation')],'namespace %s %s')

nn('NamespaceInitialisation')
ac('Empty')
ac('StringLiteral','= %s')
ac('PrimaryName','= %s')

nn('TypeDefinition')
ac([('Identifier'),('TypeSignature'),('TypeInitialisation')],'type %s %s %s')

nn('TypeInitialisation')
ac('TypeExpression','= %s')

nn('PackageDefinition')
ac([('PackageAttributes'),('PackageNameOpt'),('PackageBody')],'%s package %s %s')

nn('PackageAttributes')
ac('','internal',True)
ac('Empty')

nn('PackageNameOpt')
ac('Empty')
ac('PackageName')

nn('PackageName')
ac('Identifier')
ac([('PackageName'),('Identifier')],'%s . %s')

nn('PackageBody')
ac(('Block',gl))

nn('UnitDefinition')
ac([('UnitName'),('UnitBody')],'unit %s %s')

nn('UnitName')
ac('Identifier')
ac([('UnitName'),('Identifier')],'%s . %s')

nn('UnitBody')
ac(('Block',gl))

## PRAGMAS ##

nn('Pragmas')
ac('Pragma')
ac([('Pragmas'),('Pragma')])

nn('Pragma')
ac([('UsePragma'),('Semicolon',full)])
ac([('ImportPragma'),('Semicolon',full)])

nn('UsePragma')
ac('PragmaItems','use %s')

nn('PragmaItems')
ac('PragmaItem')
ac([('PragmaItems'),('PragmaItem')],'%s , %s')

nn('PragmaItem')
ac(('LeftHandSideExpression',allowColon+allowIn),'decimal %s')
ac('PrimaryName','default namespace %s')
ac('PrimaryName','namespace %s')
ac('','standard',True)
ac('','strict',True)
ac([('UnitName'),('StringLiteral')],'unit %s %s')

nn('ImportPragma')
ac('ImportName','import %s')
ac([('Identifier'),('ImportName')],'import %s = %s')

nn('ImportName')
ac('PackageName','%s .  *')
ac([('PackageName'),('Identifier')],'%s.%s')

## BLOCKS AND PROGRAMS ##

nn('Block',t)
ac(('Directives',t), '{ %s }')

nn('Program')
ac(('Directives',gl))

nn('Empty')
ac('Empty',' ',True)

### END ###