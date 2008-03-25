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
ac(3,'','break',True)
ac(3,'','case',True)
ac(3,'','cast',True)
ac(3,'','catch',True)
ac(3,'','class',True)
ac(3,'','continue',True)
ac(3,'','debugger',True)
ac(3,'','default',True)
ac(3,'','delete',True)
ac(3,'','do',True)
ac(3,'','else',True)
ac(3,'','enum',True)
ac(3,'','extends',True)
ac(3,'','false',True)
ac(3,'','finally',True)
ac(3,'','for',True)
ac(3,'','function',True)
ac(3,'','if',True)
ac(3,'','in',True)
ac(3,'','instanceof',True)
ac(3,'','internal',True)
ac(3,'','is',True)
ac(3,'','new',True)
ac(3,'','null',True)
ac(3,'','private',True)
ac(3,'','protected',True)
ac(3,'','public',True)
ac(3,'','return',True)
ac(3,'','super',True)
ac(3,'','switch',True)
ac(3,'','this',True)
ac(3,'','throw',True)
ac(3,'','true',True)
ac(3,'','try',True)
ac(3,'','typeof',True)
ac(3,'','var',True)
ac(3,'','void',True)
ac(3,'','while',True)
ac(3,'','with',True)
ac(3,'','wrap',True)

nn('ContextuallyReservedIdentifier')
ac(3,'','const',True)
ac(3,'','decimal',True)
ac(3,'','double',True)
ac(3,'','dynamic',True)
ac(3,'','each',True)
ac(3,'','eval',True)
ac(3,'','final',True)
ac(3,'','generator',True)
ac(3,'','generic',True)
ac(3,'','get',True)
ac(3,'','has',True)
ac(3,'','implements',True)
ac(3,'','import',True)
ac(3,'','int',True)
ac(3,'','interface',True)
ac(3,'','intrinsic',True)
ac(3,'','let',True)
ac(3,'','namespace',True)
ac(3,'','native',True)
ac(3,'','Number',True)
ac(3,'','override',True)
ac(3,'','package',True)
ac(3,'','precision',True)
ac(3,'','prototype',True)
ac(3,'','rounding',True)
ac(3,'','set',True)
ac(3,'','standard',True)
ac(3,'','static',True)
ac(3,'','strict',True)
ac(3,'','to',True)
ac(3,'','type',True)
ac(3,'','uint',True)
ac(3,'','undefined',True)
ac(3,'','unit',True)
ac(3,'','use',True)
ac(3,'','xml',True)
ac(3,'','yield',True)

#TODO: Implement virtual ; behavior
nn('VirtualSemicolon')
ac(3,'',';',True)

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
ac(3,'RandomTextIdentifier',RandomTextIdentifier,True) 

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
ac(3,'RandomStringLiteral',RandomStringLiteral,True)


# NuberLiteral
nn('NumberLiteral')
ac(3,'DecimalLiteral')
ac(3,'HexIntegerLiteral')

nn('DecimalLiteral')
ac(3,[('DecimalIntegerLiteral'),('DecimalDigits'),('ExponentPart')],'%s.%s%s')
ac(3,[('DecimalIntegerLiteral'),('DecimalDigits')],'%s.%s')
ac(3,[('DecimalIntegerLiteral'),('ExponentPart')],'%s%s')
ac(3,'DecimalIntegerLiteral')
ac(3,[('DecimalDigits'),('ExponentPart')],'.%s%s')
ac(3,('DecimalDigits'),'.%s')
ac(3,[('DecimalIntegerLiteral'),('ExponentPart')],'%s%s')
ac(3,[('DecimalIntegerLiteral')])

nn('DecimalIntegerLiteral')
ac(3,'zero','0',True)
ac(3,[('NonZeroDigit'),('DecimalDigits')],'%s%s')
ac(3,'NonZeroDigit')

nn('DecimalDigits')
ac(3,'DecimalDigit')
ac(3,[('DecimalDigits'),('DecimalDigit')],'%s%s')

nn('DecimalDigit')
ac(3,'',lambda : str(random.randint(0,9)),True)

nn('NonZeroDigit')
ac(3,'',lambda : str(random.randint(1,9)),True)

nn('ExponentPart')
ac(3,[('ExponentIndicator'),('SignedInteger')],'%s%s')

nn('ExponentIndicator')
ac(3,'',lambda : random.choice('eE'), True)

nn('SignedInteger')
ac(3,'DecimalDigits')
ac(3,'DecimalDigits', '+%s')
ac(3,'DecimalDigits', '-%s')

nn('HexDigit')
ac(3,'',lambda : random.choice('0123456789abcdefABCDEF'),True)

nn('HexIntegerLiteral')
ac(3,'HexDigit','0x%s')
ac(3,'HexDigit','0X%s')
ac(3,[('HexIntegerLiteral'),('HexDigit')],'%s%s')
# End NumberLiteral

#RegularExpression
nn('RegularExpression')
ac(3,'RegularExpressionLiteral')

#[see Ecma-262 section 7.8.5]
nn('RegularExpressionLiteral')
ac(3,[('RegularExpressionBody'),('RegularExpressionFlags')],'/ %s / %s')

nn('RegularExpressionBody')
ac(3,[('RegularExpressionFirstChar'),('RegularExpressionChars')])

nn('RegularExpressionChars')
ac(3,'Empty')
ac(3,[('RegularExpressionChars'),('RegularExpressionChar')])

nn('RegularExpressionFirstChar')
#NonTerminator but not * or \ or / 
ac(3,'', lambda : replaceChars(getNodeStr('NonTerminator'),('*\\/'),'$'),True)
ac(3,'BackslashSequence')

nn('RegularExpressionChar')
#NonTerminator but not \ or / 
ac(3,'', lambda : replaceChars(getNodeStr('NonTerminator'),'\\/','$'),True)
ac(3,'BackslashSequence')

nn('BackslashSequence')
ac(3,'NonTerminator','\ %s')

nn('NonTerminator')
#SourceCharacter but not LineTerminator
#TODO: SourceCharacterr does not have any LineTerminator in it, so don't bother w/ replace
# getting some strange infinite loop w/ replaceChars code below
#ac(3,'',lambda : replaceChars(getNodeStr('SourceCharacter'),getNodeStr('LineTerminator'),'a'),True)
ac(3,'SourceCharacter')

nn('RegularExpressionFlags')
ac(3,'Empty')
ac(3,[('RegularExpressionFlags'),('IdentifierPart')])

nn('IdentifierPart')
ac(3,'', lambda : random.choice('bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$'),True)

#Add LineTerminator since that isn't specified otherwise
nn('LineTerminator')
ac(3,'linefeed','\n',True)
ac(3,'carriageReturn','\r',True)
#line separator
#paragraph separator


### XML ###
#[see Ecma-357 section 8.3]

#have to implement SourceCharacter for XML
nn('SourceCharacters')
ac(0,[('SourceCharacter'),('SourceCharacters')])
ac(0,'SourceCharacter')

nn('SourceCharacter') #changed from spec - no unicode
ac(0,'',lambda : random.choice('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_~!@#%^&*()-=+`/{}\\|;:<>,.?'),True)

nn('XMLMarkup') 
ac(0,'XMLComment')
ac(0,'XMLCDATA')
ac(0,'XMLPI')

nn('XMLTagCharacters')
# SourceCharacters but no embedded XMLTagPunctuator 
#or left-curly { or quote ' or double-quote " or forward-slash / or XMLWhitespaceCharacter 
ac(0,'',lambda : replaceChars(getNodeStr('SourceCharacters'),'{/','*'),True) #replace w/ a char to avoid empty string

nn('XMLWhitespaceCharacter')
ac(0,'',' ',True) #<SP>
ac(0,'','\t',True) #<TAB>
ac(0,'','\r',True) #<CR>
ac(0,'','\n',True) #<LF>

nn('XMLWhitespace')
ac(0,'XMLWhitespaceCharacter')
ac(0,[('XMLWhitespace'),('XMLWhitespaceCharacter')])
 
nn('XMLText')
# SourceCharacters but no embedded left-curly { or less-than < 
ac(0,'',lambda : replaceChars(getNodeStr('SourceCharacters'),'{<','*'),True)
 
#TODO: Proper Unicode chars
nn('UnicodeLetter')
ac(0,'',lambda : random.choice('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'),True)

nn('UnicodeDigit')
ac(0,'',lambda : random.choice('0123456789'),True)

nn('XMLName')
ac(0,'XMLNameStart')
ac(0,[('XMLName'),('XMLNamePart')])
 
nn('XMLNameStart')
ac(0,'UnicodeLetter')
ac(0,'underscore','_',True)
ac(0,'colon',':',True)
 
nn('XMLNamePart')
ac(0,'UnicodeLetter')
ac(0,'UnicodeDigit')
ac(0,'period','.',True) 
ac(0,'hyphen','-',True) 
ac(0,'underscore','_',True)
ac(0,'colon',':',True) 
 
nn('XMLComment')
ac(0,'emptyComments','<!--  -->',True)
ac(0,'XMLCommentCharacters','<!-- %s -->')
 
nn('XMLCommentCharacters')
# SourceCharacters but no embedded sequence -- 
ac(0,'',lambda : getNodeStr('SourceCharacters').replace('--','$'),True)
 
nn('XMLCDATA')
ac(0,'','<![CDATA[  ]]>',True)
ac(0,'XMLCDATACharacters','<![CDATA[ %s ]]>')
 
nn('XMLCDATACharacters')
#SourceCharacters but no embedded sequence ]]> 
ac(0,'',lambda : getNodeStr('SourceCharacters').replace(']]>','$'),True)
 
nn('XMLPI')
ac(0,'','<?  ?>',True)
ac(0,'XMLPICharacters','<? %s ?>')
 
nn('XMLPICharacters')
#SourceCharacters but no embedded sequence ?>
ac(0,'',lambda : getNodeStr('SourceCharacters').replace('?>','$'),True)
 
nn('XMLAttributeValue')
#" XMLDoubleStringCharactersopt " 
ac(0,'XMLDoubleStringCharacters','" %s "')
#' XMLSingleStringCharactersopt ' 
ac(0,'XMLSingleStringCharacters',"' %s '")
 
nn('XMLDoubleStringCharacters')
#SourceCharacters but no embedded double-quote " 
ac(0,'',lambda : getNodeStr('SourceCharacters').replace('"','$'),True)
 
nn('XMLSingleStringCharacters')
#SourceCharacters but no embedded single-quote ' 
ac(0,'',lambda : getNodeStr('SourceCharacters').replace("'",'$'),True)

## End XML

### Syntactic Structure ###

## Expressions ##
nn('Identifier')
ac(3,'LexIdentifier') #Note that this is changed from grammar.xls from bold Identifier to LexIdentifier
ac(3,'ContextuallyReservedIdentifier')

nn('Qualifier')
ac(0,'','*',True)
ac(3,'Identifier')
ac(4,'ReservedNamespace')

nn('ReservedNamespace')
ac(4,'',lambda : random.choice('internal intrinsic private protected public'.split()),True)

nn('QualifiedNameIdentifier')
ac(4,'Identifier')
ac(4,'ReservedIdentifier')
ac(4,'StringLiteral')
ac(4,'NumberLiteral')
ac(4,'Brackets')
ac(4,'OverloadedOperator')

nn('QualifiedName')
ac(3,'Identifier')
ac(4,[('Qualifier'),('QualifiedNameIdentifier')],'%s :: %s')
ac(4,[('ParenListExpression'),('QualifiedNameIdentifier')],'%s :: %s')
ac(4,[('QualifiedName'),('QualifiedNameIdentifier')],'%s :: %s')

nn('AttributeName')
ac(0,'Brackets','@ %s')
ac(0,'QualifiedName','@ %s')

nn('PropertyName')
ac(0,'AttributeName')
ac(4,'QualifiedName')

nn('PrimaryName')
ac(4,[('Path'),('QualifiedName')],'%s . %s')
ac(3,'QualifiedName')
ac(0,'AttributeName')

nn('Path')
ac(4,'Identifier')
ac(4,[('Path'),('Identifier')],'%s . %s')

nn('ParenExpression')
ac(3,('AssignmentExpression',allowColon+allowIn),'( %s )')

nn('ParenListExpression')
ac(3,('ListExpression',allowColon+allowIn), '( %s )')

nn('FunctionExpression',a+b)
ac(3,[('Identifier'),('FunctionSignature'),('FunctionExpressionBody', a+b)],'function %s %s %s')
ac(3,[('FunctionSignature'),('FunctionExpressionBody', a+b)],'function %s %s')

nn('FunctionExpressionBody', a+b)
ac(3,('Block',local))
ac(4,('AssignmentExpression', a+b))

nn('ObjectLiteral',noColon)
ac(3,'FieldList','{ %s }')

nn('ObjectLiteral',allowColon)
ac(3,'FieldList','{ %s }')
ac(4,[('FieldList'),('TypeExpression')],'{ %s } : %s')

nn('FieldList')
ac(3,'Empty')
ac(3,'LiteralField')
ac(3,[('LiteralField'),('FieldList')],'%s , %s')

nn('LiteralField')
ac(3,[('FieldKind'),('FieldName'),('AssignmentExpression', allowColon+allowIn)],'%s %s : %s')
ac(4,[('FieldName'),('FunctionSignature'),('FunctionExpressionBody', allowColon+allowIn)],'get %s %s %s')
ac(4,[('FieldName'),('FunctionSignature'),('FunctionExpressionBody', allowColon+allowIn)],'set %s %s %s')

nn('FieldKind')
ac(3,'Empty')
ac(4,'','const',True)

nn('FieldName')
ac(3,'QualifiedName')
ac(3,'StringLiteral')
ac(3,'NumberLiteral')
ac(4,'ReservedIdentifier')

nn('ArrayLiteral',noColon)
ac(3,'Elements','[ %s ]')

nn('ArrayLiteral',allowColon)
ac(3,'Elements','[ %s ]')
ac(4,[('Elements'),('TypeExpression')], '[ %s ] : %s')

nn('Elements')
ac(3,'ElementList')
ac(4,'ElementComprehension')

nn('ElementList')
ac(3,'Empty')
ac(3,'LiteralElement')
ac(3,'ElementList',', %s')
ac(3,[('LiteralElement'),('ElementList')],'%s, %s')

nn('LiteralElement')
ac(3,('AssignmentExpression', allowColon+allowIn))

nn('ElementComprehension')
ac(4,[('LiteralElement'),('ForInExpressionList'),('OptionalIfCondition')])

nn('ForInExpressionList')
ac(4,'ForInExpression')
ac(4,[('ForInExpressionList'),('ForInExpression')])

nn('ForInExpression')
ac(3,[('ForInBinding'),('ListExpression',allowColon+allowIn)],'for ( %s in %s )')
ac(4,[('ForInBinding'),('ListExpression',allowColon+allowIn)],'for each ( %s in %s )')

nn('OptionalIfCondition')
ac(4,'Empty')
ac(4,'ParenListExpression','if %s')


nn('XMLInitialiser')
ac(0,'XMLMarkup')
ac(0,'XMLElement')
ac(0,'XMLElementContent','< > %s </ >')

nn('XMLElementContent')
ac(0,[('ListExpression',allowColon+allowIn),('XMLElementContent')],'{ %s } %s')
ac(0,[('XMLMarkup'),('XMLElementContent')])
ac(0,[('XMLText'),('XMLElementContent')])
ac(0,[('XMLElement'),('XMLElementContent')])
ac(0,'Empty')

nn('XMLElement')
ac(0,[('XMLTagContent'),('XMLWhitespace')],'< %s %s />')
ac(0,'XMLTagContent','< %s />')
ac(0,[('XMLTagContent'),('XMLWhitespace'),('XMLElementContent'),('XMLTagName'),('XMLWhitespace')],
    '< %s %s > %s </ %s %s >')
ac(0,[('XMLTagContent'),('XMLElementContent'),('XMLTagName'),('XMLWhitespace')],
    '< %s > %s </ %s %s >')
ac(0,[('XMLTagContent'),('XMLWhitespace'),('XMLElementContent'),('XMLTagName')],
    '< %s %s > %s </ %s >')
ac(0,[('XMLTagContent'),('XMLElementContent'),('XMLTagName')],'< %s > %s </ %s >')


nn('XMLTagContent')
ac(0,[('XMLTagName'),('XMLAttributes')])

nn('XMLTagName')
ac(0,('ListExpression',allowColon+allowIn),'{ %s }')
ac(0,'XMLName')

nn('XMLAttributes')
ac(0,[('XMLWhitespace'),('ListExpression',allowColon+allowIn)],'%s { %s }')
ac(0,[('XMLAttribute'),('XMLAttributes')])
ac(0,'Empty')

nn('XMLAttribute')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s %s = %s { %s }')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s = %s { %s }')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('ListExpression',allowColon+allowIn)],
    '%s %s %s = { %s }')
ac(0,[('XMLWhitespace'),('XMLName'),('ListExpression',allowColon+allowIn)],'%s %s = { %s }')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLWhitespace'),('XMLAttributeValue')],
    '%s %s %s = %s %s')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLAttributeValue')],'%s %s = %s %s')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLWhitespace'),('XMLAttributeValue')],'%s %s %s = %s')
ac(0,[('XMLWhitespace'),('XMLName'),('XMLAttributeValue')],'%s %s = %s')

nn('PrimaryExpression', a+b)
ac(3,'null','null',True)
ac(3,'true','true',True)
ac(3,'false','false',True)
ac(3,'NumberLiteral')
ac(3,'StringLiteral')
ac(3,'RegularExpression')
ac(0,'XMLInitialiser')
ac(3,('ArrayLiteral',a))
ac(3,('ObjectLiteral',a))
ac(3,('FunctionExpression', a+b))
ac(3,'ThisExpression')
ac(3,'ParenListExpression')
ac(3,'PrimaryName')

nn('ThisExpression')
ac(3,'this','this',True)
ac(4,'this function', 'this function',True)
ac(4,'this generator','this generator',True)

nn('SuperExpression')
ac(4,'super','super',True)
ac(4,'ParenExpression','super %s')

nn('Arguments')
ac(3,'','( )',True)
ac(3,'ArgumentList','( %s )')

nn('ArgumentList')
ac(3,('AssignmentExpression', allowColon+allowIn))
ac(3,[('ArgumentList'),('AssignmentExpression', allowColon+allowIn)],'%s, %s')

nn('PropertyOperator')
ac(4,'ReservedIdentifier','.%s')
ac(3,'PropertyName','.%s')
ac(0,'QualifiedName','..%s')
ac(0,'ParenListExpression','.%s')
ac(4,[('ParenListExpression'),('QualifiedNameIdentifier')],'.%s::%s')
ac(3,'Brackets')
ac(4,'TypeApplication')

nn('Brackets')
ac(3,('ListExpression', allowColon+allowIn),'[ %s ]')
ac(4,'SliceExpression', '[ %s ]')
	
nn('SliceExpression')
ac(4,[('OptionalExpression'),('OptionalExpression')],'%s : %s')
ac(4,[('OptionalExpression'),('OptionalExpression'),('OptionalExpression')],'%s : %s :%s')
	
nn('OptionalExpression')
ac(4,('ListExpression', allowColon+allowIn))
ac(4,'Empty')

nn('TypeApplication')
ac(4,'TypeExpressionList','.< %s >')
	
nn('MemberExpression',a+b)	
ac(3,('PrimaryExpression', a+b))
ac(3,[('MemberExpression',a+b),('Arguments')],'new %s %s')
ac(4,[('SuperExpression'),('PropertyOperator')])
ac(3,[('MemberExpression',a+b),('PropertyOperator')])
	
nn('CallExpression', a+b)	
ac(3,[('MemberExpression',a+b),('Arguments')])
ac(3,[('CallExpression',a+b),('Arguments')])
ac(3,[('CallExpression',a+b),('PropertyOperator')])

nn('NewExpression',a+b)
ac(3,('MemberExpression',a+b))
ac(3,('NewExpression',a+b),'new %s')

nn('LeftHandSideExpression',a+b)
ac(3,('NewExpression',a+b))
ac(3,('CallExpression',a+b))
	
nn('UnaryTypeExpression',a+b)
ac(3,('LeftHandSideExpression',a+b))
ac(4,'TypeExpression','type %s')
	
nn('PostfixExpression',a+b)	
ac(4,('UnaryTypeExpression',a+b))
ac(3,('LeftHandSideExpression', a+b),'%s ++')    #LeftHandSideExpressiona, b  [no line break]  ++
ac(3,('LeftHandSideExpression', a+b),'%s --')    #LeftHandSideExpressiona, b  [no line break]  --
	
nn('UnaryExpression', a+b)
ac(3,('PostfixExpression',a+b))
ac(3,('PostfixExpression', a+b),'delete %s')
ac(3,('UnaryExpression', a+b),'void %s')
ac(3,('UnaryExpression', a+b),'typeof %s')
ac(3,('PostfixExpression', a+b),'++ %s')
ac(3,('PostfixExpression', a+b),'-- %s')
ac(3,('UnaryExpression', a+b),'+ %s')
ac(3,('UnaryExpression', a+b),'- %s')
ac(3,('UnaryExpression', a+b),'~ %s')
ac(3,('UnaryExpression', a+b),'! %s')

	
nn('MultiplicativeExpression',a+b)
ac(3,('UnaryExpression', a+b))
ac(3,[('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s * %s')
ac(3,[('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s / %s')
ac(3,[('MultiplicativeExpression', a+b),('UnaryExpression', a+b)],'%s %% %s') #Note double %% to return % after formatting

nn('AdditiveExpression',a+b)
ac(3,('MultiplicativeExpression', a+b))
ac(3,[('AdditiveExpression', a+b),('MultiplicativeExpression', a+b)],'%s + %s')
ac(3,[('AdditiveExpression', a+b),('MultiplicativeExpression', a+b)],'%s - %s')

nn('ShiftExpression',a+b)
ac(3,('AdditiveExpression', a+b))
ac(3,[('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s << %s')
ac(3,[('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s >> %s')
ac(3,[('ShiftExpression', a+b),('AdditiveExpression', a+b)],'%s >>> %s')

nn('RelationalExpression',a+allowIn)
ac(3,('ShiftExpression', b))
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s < %s')
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s > %s')
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s <= %s')
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s >= %s')
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s in %s')
ac(3,[('RelationalExpression', a+allowIn),('ShiftExpression', a+b)],'%s instanceof %s')
ac(4,[('RelationalExpression', a+allowIn),('TypeExpression')],'%s cast %s')
ac(4,[('RelationalExpression', a+allowIn),('TypeExpression')],'%s is %s')
ac(4,[('RelationalExpression', a+allowIn),('TypeExpression')],'%s wrap %s')

nn('RelationalExpression',a+noIn)
ac(3,('ShiftExpression', a+b))
ac(3,[('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s < %s')
ac(3,[('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s > %s')
ac(3,[('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s <= %s')
ac(3,[('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s >= %s')
ac(3,[('RelationalExpression', a+noIn),('ShiftExpression', a+b)],'%s instanceof %s')
ac(4,[('RelationalExpression', a+noIn),('TypeExpression')],'%s cast %s')
ac(4,[('RelationalExpression', a+noIn),('TypeExpression')],'%s is %s')
ac(4,[('RelationalExpression', a+noIn),('TypeExpression')],'%s wrap %s')

nn('EqualityExpression',a+b)
ac(3,('RelationalExpression', a+b))
ac(3,[('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s == %s')
ac(3,[('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s != %s')
ac(3,[('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s === %s')
ac(3,[('EqualityExpression', a+b),('RelationalExpression', a+b)],'%s !== %s')

nn('BitwiseAndExpression',a+b)
ac(3,('EqualityExpression',a+b))
ac(3,[('BitwiseAndExpression', a+b),('EqualityExpression', a+b)],'%s & %s')

nn('BitwiseXorExpression',a+b)
ac(3,('BitwiseAndExpression',a+b))
ac(3,[('BitwiseXorExpression', a+b),('BitwiseAndExpression', a+b)],'%s ^ %s')	

nn('BitwiseOrExpression',a+b)
ac(3,('BitwiseXorExpression',a+b))
ac(3,[('BitwiseOrExpression', a+b),('BitwiseXorExpression', a+b)],'%s | %s')		

nn('LogicalAndExpression',a+b)
ac(3,('BitwiseOrExpression',a+b))
ac(3,[('LogicalAndExpression', a+b),('BitwiseOrExpression', a+b)],'%s && %s')		

nn('LogicalOrExpression',a+b)
ac(3,('LogicalAndExpression',a+b))
ac(3,[('LogicalOrExpression', a+b),('LogicalOrExpression', a+b)],'%s || %s')		

nn('ConditionalExpression',a+b)
ac(4,('LetExpression',a+b))
ac(4,('YieldExpression',a+b))
ac(3,('LogicalOrExpression',a+b))
ac(3,[('LogicalOrExpression', a+b),('AssignmentExpression',noColon+b),('AssignmentExpression', a+b)],'%s ? %s : %s')		

nn('NonAssignmentExpression',a+b)
ac(4,('LetExpression',a+b))
ac(4,('YieldExpression',a+b))
ac(3,('LogicalOrExpression',a+b))
ac(3,[('LogicalOrExpression', a+b),('NonAssignmentExpression',noColon+b),('NonAssignmentExpression', a+b)],'%s ? %s : %s')	

nn('LetExpression',a+b)
ac(4,[('LetBindingList'),('AssignmentExpression', a+b)],'let ( %s ) %s')
	
nn('LetBindingList')
ac(4,'Empty')
ac(4,'NonemptyLetBindingList')

nn('NonemptyLetBindingList')
ac(4,('VariableBinding',allowIn))
ac(4,[('VariableBinding',allowIn),('NonemptyLetBindingList')],'%s, %s')
	
nn('YieldExpression',a+b)
ac(4,'yield','yield',True)
ac(4,('AssignmentExpression',a+b),'yield %s') #yield  [no line break]  AssignmentExpressiona, b
	
nn('AssignmentExpression',a+b)	
ac(3,('ConditionalExpression',a+b))
ac(3,[('Pattern',a+b+allowExpr),('AssignmentExpression',a+b)],'%s = %s')
ac(3,[('SimplePattern',a+b+allowExpr),('CompoundAssignmentOperator'),('AssignmentExpression',a+b)])
	
nn('CompoundAssignmentOperator')
ac(3,'',lambda : random.choice('*= /= %= += -= <<= >>= >>>= &= ^= |= &&= ||='.split(' ')),True)

nn('ListExpression',a+b)	
ac(3,('AssignmentExpression',a+b))
ac(3,[('ListExpression',a+b),('AssignmentExpression',a+b)],'%s, %s')

## Patterns ##
nn('Pattern', a+b+g)	
ac(3,('SimplePattern',a+b+g))
ac(4,('ObjectPattern', a+b+g))
ac(4,('ArrayPattern',g))
	
nn('SimplePattern',a+b+noExpr)
ac(3,'Identifier')
	
nn('SimplePattern',a+b+allowExpr)
ac(3,('LeftHandSideExpression',a+b))
	
nn('ObjectPattern',g)	
ac(4,('FieldListPattern',g),'{ %s }')
	
nn('FieldListPattern',g)
ac(4,'Empty')
ac(4,('FieldPattern',g))
ac(4,[('FieldListPattern',g),('FieldPattern',g)],'%s, %s')
	
nn('FieldPattern',g)
ac(4,'FieldName')
ac(4,[('FieldName'),('Pattern',allowColon+allowIn+g)],'%s : %s')
	
nn('ArrayPattern',g)
ac(4,('ElementListPattern',g),'[ %s ]')
	
nn('ElementListPattern',g)
ac(4,'Empty')
ac(4,('ElementPattern',g))
ac(4,('ElementListPattern',g),', %s')
ac(4,[('ElementPattern',g),('ElementListPattern',g)],'%s, %s')

nn('ElementPattern',g)
ac(4,('Pattern',allowColon+allowIn+g))
	
nn('TypedIdentifier')
ac(3,('SimplePattern',allowColon+allowIn+noExpr))
ac(4,[('SimplePattern',allowColon+allowIn+noExpr),('TypeAnnotation')],'%s : %s')
	
nn('TypedPattern',b)	
ac(3,('Pattern',allowColon+b+noExpr))
ac(4,[('Pattern',allowColon+b+noExpr),('TypeAnnotation')],'%s : %s')
	
nn('TypeAnnotation')
ac(4,'TypeExpression')
ac(4,'TypeExpression','wrap %s')

## Type Expressions ##
nn('TypeExpression')
ac(4,'NullableTypeExpression')
ac(4,'NullableTypeExpression','like %s')

nn('NullableTypeExpression')
ac(4,'BasicTypeExpression')
ac(4,'BasicTypeExpression','%s ?')
ac(4,'BasicTypeExpression','%s !')

nn('BasicTypeExpression')
ac(4,'*','*',True)
ac(4,'null','null',True)
ac(4,'undefined','undefined',True)
ac(4,'FunctionType')
ac(4,'UnionType')
ac(4,'RecordType')
ac(4,'ArrayType')
ac(4,'PrimaryName')

nn('FunctionType')
ac(4,'FunctionSignatureType','function %s')

nn('FunctionSignatureType')
ac(4,[('TypeParameters'),('ParametersType'),('ResultType')],'%s ( %s ) %s')
ac(4,[('TypeParameters'),('PrimaryName'),('ResultType')],'%s ( this : %s ) %s')
ac(4,[('TypeParameters'),('PrimaryName'),('NonemptyParametersType'),('ResultType')],'%s ( this: %s, %s ) %s')

nn('ParametersType')
ac(4,'Empty')
ac(4,'NonemptyParametersType')

nn('NonemptyParametersType')
ac(4,'ParameterInitType')
ac(4,[('ParameterInitType'),('NonemptyParametersType')],'%s, %s')
ac(4,'RestParameterType')

nn('ParameterInitType')
ac(4,'ParameterType')
ac(4,'ParameterType','%s =')

nn('ParameterType')
ac(4,'TypeExpression')

nn('RestParameterType')
ac(4,'','...', True)
ac(4,'ParameterType','... %s')

nn('UnionType')
ac(4,'TypeExpressionList','( %s )')

nn('RecordType')
ac(4,'FieldTypeList','{ %s }')

nn('FieldTypeList')
ac(4,'Empty')
ac(4,'NonemptyFieldTypeList')

nn('NonemptyFieldTypeList')
ac(4,'FieldType')
ac(4,[('FieldType'),('NonemptyFieldTypeList')],'%s, %s')

nn('FieldType')
ac(4,[('FieldName'),('TypeExpression')],'%s : %s')

nn('ArrayType')
ac(4,'ElementTypeList','[ %s ]')

nn('ElementTypeList')
ac(4,'Empty')
ac(4,'TypeExpression')
ac(4,'ElementTypeList',', %s')
ac(4,[('TypeExpression'),('ElementTypeList')],'%s, %s')

nn('TypeExpressionList')
ac(4,'TypeExpression')
ac(4,[('TypeExpressionList'),('TypeExpression')],'%s, %s')

### Statements ###
nn('Statement',t+w)
ac(3,('BlockStatement',t))
ac(3,[('BreakStatement'),('Semicolon',w)])
ac(3,[('ContinueStatement'),('Semicolon',w)])
ac(0,[('DefaultXMLNamespaceStatement'),('Semicolon',w)])
ac(3,[('DoStatement'),('Semicolon',w)])
ac(3,[('ExpressionStatement'),('Semicolon',w)])
ac(3,('ForStatement',w))
ac(3,('IfStatement',w))
ac(3,('LabeledStatement',w))
ac(4,('LetStatement',w))
ac(3,[('ReturnStatement'),('Semicolon',w)])
ac(3,'SwitchStatement')
ac(4,'SwitchTypeStatement')
ac(3,[('ThrowStatement'),('Semicolon',w)])
ac(3,'TryStatement')
ac(3,('WhileStatement',w))
ac(3,('WithStatement',w))

nn('Substatement',w)
ac(3,'EmptyStatement')
ac(3,('Statement',local+w))

nn('Semicolon',abbrev) 
ac(3,';',';',True)
ac(3,'VirtualSemicolon')
ac(3,'Empty')

nn('Semicolon',noShortIf)
ac(3,'',';',True)
ac(3,'VirtualSemicolon')
ac(3,'Empty')

nn('Semicolon',full)
ac(3,'',';',True)
ac(3,'VirtualSemicolon')

nn('EmptyStatement')
ac(3,'',';',True)

nn('ExpressionStatement')
ac(3,('ListExpression',allowColon+allowIn))
##TODO: whats lookahead?
##[lookahead !{ function, let, { }] ListExpressionallowColon, allowIn
	
nn('BlockStatement',t)
ac(3,('Block',t))
	
nn('LabeledStatement',w)	
ac(3,[('Identifier'),('Substatement',w)])
	
nn('IfStatement',abbrev)
ac(3,[('ParenListExpression'),('Substatement',abbrev)],'if %s %s')
ac(3,[('ParenListExpression'),('Substatement',noShortIf),('Substatement',abbrev)],'if %s %s else %s')

nn('IfStatement',full)
ac(3,[('ParenListExpression'),('Substatement',full)],'if %s %s')
ac(3,[('ParenListExpression'),('Substatement',noShortIf),('Substatement',full)],'if %s %s else %s')

nn('IfStatement',noShortIf)
ac(3,[('ParenListExpression'),('Substatement',noShortIf),('Substatement',noShortIf)],'if %s %s else %s')

nn('WithStatement',w)
ac(3,[('TypedExpression'),('Substatement',w)],'with %s %s')

nn('TypedExpression')
ac(3,'ParenListExpression')
ac(4,[('ParenListExpression'),('TypeExpression')],'%s : %s')

nn('SwitchStatement')
ac(3,[('ParenListExpression'),('CaseElements')],'switch %s { %s }')

nn('CaseElements')
ac(3,'Empty')
ac(3,'CaseLabel')
ac(3,[('CaseLabel'),('CaseElementsPrefix'),('CaseLabel')])
ac(3,[('CaseLabel'),('CaseElementsPrefix'),('Directive',local+abbrev)])

nn('CaseElementsPrefix')
ac(3,'Empty')
ac(3,[('CaseElementsPrefix'),('CaseLabel')])
ac(3,[('CaseElementsPrefix'),('Directive',local+abbrev)])

nn('CaseLabel')
ac(3,('ListExpression',allowColon+allowIn),'case %s :')
ac(3,'','default :',True)

nn('SwitchTypeStatement')
ac(4,[('TypedExpression'),('TypeCaseElements')],'switch type %s { %s }')

nn('TypeCaseElements')
ac(4,'TypeCaseElement')
ac(4,[('TypeCaseElements'),('TypeCaseElement')])

nn('TypeCaseElement')
ac(4,[('TypedPattern',allowColon+allowIn) ,('Block',local)],'case ( %s ) %s')

nn('DoStatement')
ac(3,[('Substatement',abbrev),('ParenListExpression')],'do %s while %s')

nn('WhileStatement',w)
ac(3,[('ParenListExpression'),('Substatement',w)],'while %s %s')

nn('ForStatement',w)
ac(3,[('ForInitialiser'),('OptionalExpression'),('OptionalExpression'),('Substatement',w)],'for ( %s; %s; %s) %s')
ac(3,[('ForInBinding'),('ListExpression',allowColon+allowIn),('Substatement',w)],'for ( %s in %s) %s')
ac(4,[('ForInBinding'),('ListExpression',allowColon+allowIn),('Substatement',w)],'for each ( %s in %s) %s')

nn('ForInitialiser')
ac(3,'Empty')
ac(3,('ListExpression',allowColon+noIn))
ac(3,('VariableDefinition',noIn))

nn('ForInBinding')
ac(3,('Pattern',allowColon+noIn+allowExpr))
ac(3,[('VariableDefinitionKind'),('VariableBinding',noIn)])

nn('LetStatement',w)
ac(4,[('LetBindingList'),('Block',local)],'let ( %s ) %s')

nn('ContinueStatement')
ac(3,'','continue',True)
ac(3,'Identifier','continue %s')

nn('BreakStatement')
ac(3,'','break',True)
ac(3,'Identifier','break %s')

nn('ReturnStatement')
ac(3,'','return',True)
ac(3,('ListExpression',allowColon+allowIn),'return %s')

nn('ThrowStatement')
ac(3,('ListExpression',allowColon+allowIn),'throw %s')

nn('TryStatement')
ac(3,[('Block',local),('CatchClauses')],'try %s %s')
ac(3,[('Block',local),('CatchClauses'),('Block',local)],'try %s %s finally %s')
ac(3,[('Block',local),('Block',local)],'try %s finally %s')

nn('CatchClauses')
ac(3,'CatchClause')
ac(3,[('CatchClauses'),('CatchClause')])

nn('CatchClause')
ac(3,[('Parameter'),('Block',local)],'catch ( %s ) %s')

nn('DefaultXMLNamespaceStatement')
ac(0,('NonAssignmentExpression',allowColon+allowIn),'default xml namespace = %s')

## DIRECTIVES ##    

#t = { global, class, interface, local }

nn('Directives',t)
ac(3,'Empty')
ac(3,[('DirectivesPrefix',t),('Directive',t+abbrev)])

nn('DirectivesPrefix',t)
ac(3,'Empty')
ac(4,'Pragmas')
ac(3,[('DirectivesPrefix',t),('Directive',t+full)])

nn('Directive',t+w)
ac(3,'EmptyStatement')
ac(3,('Statement',t+w))
ac(3,('AnnotatableDirective',t+w))

nn('AnnotatableDirective',gl+w)
ac(4,[('Attribute',gl),('AnnotatableDirective',gl+w)])
ac(3,[('VariableDefinition',allowIn+w),('Semicolon',w)])
ac(3,('FunctionDefinition',gl+w))
ac(4,'ClassDefinition')
ac(4,'InterfaceDefinition')
ac(4,[('NamespaceDefinition'),('Semicolon',w)])
ac(4,[('TypeDefinition'),('Semicolon',w)])
ac(4,'PackageDefinition')
ac(4,'UnitDefinition')

nn('AnnotatableDirective',cl+w)
ac(4,[('Attribute',gl),('AnnotatableDirective',cl+w)],'%s %s',nolb=[1])
ac(3,('VariableDefinition',allowIn+w))
ac(3,('FunctionDefinition',cl+w))
ac(4,('NamespaceDefinition',w))
ac(4,('TypeDefinition',w))

nn('AnnotatableDirective',interface+w)
ac(4,[('Attribute',interface),('AnnotatableDirective',interface+w)],'%s %s',nolb=[1])
ac(4,('FunctionDeclaration',w))
ac(4,('TypeDefinition',w))

nn('AnnotatableDirective',local+w)
ac(3,('VariableDefinition',allowIn+w))
ac(3,('FunctionDefinition',local+w))
ac(4,('NamespaceDefinition',w))
ac(4,('TypeDefinition',w))

nn('Attribute',gl)
ac(4,('NamespaceAttribute',gl))
ac(4,'','dynamic',True)
ac(4,'','final',True)
ac(4,'','native',True)
ac(4,('AssignmentExpression',allowColon+allowIn),'[ %s ]')

nn('Attribute',cl)
ac(4,('NamespaceAttribute',cl))
ac(4,'','final',True)
ac(4,'','native',True)
ac(4,'','override',True)
ac(4,'','prototype',True)
ac(4,'','static',True)
ac(4,('AssignmentExpression',allowColon+allowIn),'[ %s ]')

nn('Attribute',interface)
ac(4,'NamespaceAttribute')

nn('Attribute',local)
ac(4,'NamespaceAttribute')

nn('NamespaceAttribute',gl)
ac(4,'','public',True)
ac(4,'','internal',True)
ac(4,'','intrinsic',True)
ac(4,'PrimaryName')

nn('NamespaceAttribute',cl)
ac(4,'ReservedNamespace')
ac(4,'PrimaryName')

## DEFINITIONS ##

nn('VariableDefinition',b+w)
ac(3,[('VariableDefinitionKind'),('VariableBindingList',b)])

nn('VariableDefinitionKind')
ac(4,'','const',True)
ac(4,'','let',True)
ac(4,'','let const',True)
ac(3,'','var',True)

nn('VariableBindingList',b)
ac(3,('VariableBinding',b))
ac(3,[('VariableBindingList',b),('VariableBinding',b)])

nn('VariableBinding',b)
ac(3,'TypedIdentifier')
ac(3,[('TypedPattern',b),('VariableInitialisation',b)])

nn('VariableInitialisation',b)
ac(3,('AssignmentExpression',allowColon+b),'= %s')

nn('FunctionDeclaration')
ac(4,[('FunctionName'),('FunctionSignature')],'function %s %s')

nn('FunctionDefinition',cl+w)
#function  Identifier  [identifier == outer classname]  ConstructorSignature  Blocklocal
def fd():
    #print '********outerClassName: '+outerClassName
    return 'function %s %s %s' % (outerClassName, getNodeStr('ConstructorSignature'),getNodeStr('Block',local))
#ac('',lambda : 'function %s %s %s' % 
#    (outerClassName, getNodeStr('ConstructorSignature'),getNodeStr('Block',local)),True)
ac(4,'',fd,True)
ac(4,[('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'function %s %s %s')


nn('FunctionDefinition',t+w)
ac(3,[('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'function %s %s %s')
ac(4,[('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'let function %s %s %s')
ac(4,[('FunctionName'),('FunctionSignature'),('FunctionBody',allowIn+w)],'const function %s %s %s')

nn('FunctionName')
ac(3,'Identifier')
ac(4,'OverloadedOperator')
ac(4,'Identifier','get %s')
ac(4,'Identifier','set %s')

nn('OverloadedOperator')
ac(4,'',lambda : random.choice('+ - ~ * / % < > <= >= == << >> >>> & | === != !=='.split()),True)

nn('FunctionSignature')
ac(3,[('TypeParameters'),('Parameters'),('ResultType')],'%s ( %s ) %s')
ac(4,[('TypeParameters'),('PrimaryName'),('ResultType')],'%s ( this : %s ) %s')
ac(4,[('TypeParameters'),('PrimaryName'),('NonemptyParameters'),('ResultType')],'%s ( this : %s, %s ) %s')

nn('TypeParameters')
ac(3,'Empty')
ac(4,'TypeParameterList','.< %s >')  

nn('TypeParameterList')
ac(4,'Identifier')
ac(4,[('Identifier'),('TypeParameterList')],'%s, %s')

nn('Parameters')
ac(3,'Empty')
ac(3,'NonemptyParameters')

nn('NonemptyParameters')
ac(3,'ParameterInit')
ac(3,[('ParameterInit'),('NonemptyParameters')],'%s, %s')
ac(4,'RestParameter')

nn('ParameterInit')
ac(3,'Parameter')
ac(4,[('Parameter'),('NonAssignmentExpression',allowIn)],'%s = %s')

nn('Parameter')
ac(3,[('ParameterKind'),('TypedPattern',allowIn)])

nn('ParameterKind')
ac(3,'Empty')
ac(4,'','const',True)

nn('RestParameter')
ac(4,'','...',True)
ac(4,'Parameter','... %s')

nn('ResultType')
#TODO - empty changed to 3 due to FunctionSig issue in grammar
ac(3,'Empty')
ac(4,'',':  void',True)
ac(4,'TypeAnnotation',': %s')

nn('ConstructorSignature')
ac(4,[('TypeParameters'),('Parameters')],'%s ( %s )')
ac(4,[('TypeParameters'),('Parameters'),('ConstructorInitialiser')],'%s ( %s ) : %s')

nn('ConstructorInitialiser')
ac(4,'InitialiserList')
ac(4,[('InitialiserList'),('SuperInitialiser')])
ac(4,'SuperInitialiser')

nn('InitialiserList')
ac(4,'Initialiser')
ac(4,[('InitialiserList'),('Initialiser')],'%s , %s')

nn('Initialiser')
ac(4,[('Pattern',allowIn+allowExpr),('VariableInitialisation',allowIn)])

nn('SuperInitialiser')
ac(4,'Arguments','super %s')

nn('FunctionBody',a+b+w)
ac(3,('Block',local))
ac(4,[('AssignmentExpression',a+b),('Semicolon',w)])

nn('ClassDefinition')
#The class identifier must be saved so that it can be reused for the constructor function in FunctionDefinition, class+w
def classDefinition():
    global outerClassName
    outerClassName = getNodeStr('Identifier')
    #print 'outerClassName: '+outerClassName
    return 'class %s %s %s %s' % (outerClassName,getNodeStr('TypeSignature'),getNodeStr('ClassInheritance'),getNodeStr('ClassBody'))
ac(4,'',classDefinition,True)


nn('TypeSignature')
ac(4,'TypeParameters')
ac(4,'TypeParameters','%s !')

nn('ClassInheritance')
ac(4,'Empty')
ac(4,'TypeReference','extends %s')
ac(4,'TypeReferenceList','implements %s')
ac(4,[('TypeReference'),('TypeReferenceList')],'extends %s implements %s')

nn('TypeReferenceList')
ac(4,'TypeReference')
ac(4,[('TypeReference'),('TypeReferenceList')],'%s , %s')

nn('TypeReference')
ac(4,'PrimaryName')
ac(4,[('PrimaryName'),('TypeApplication')])

nn('ClassBody')
ac(4,('Block',cl))

nn('InterfaceDefinition')
ac(4,[('Identifier'),('TypeSignature'),('InterfaceInheritance'),('InterfaceBody')],'interface  %s %s %s %s')

nn('InterfaceInheritance')
ac(4,'Empty')
ac(4,'TypeReferenceList','extends %s')

nn('InterfaceBody')
ac(4,('Block',interface))

nn('NamespaceDefinition')
ac(4,[('Identifier'),('NamespaceInitialisation')],'namespace %s %s')

nn('NamespaceInitialisation')
ac(4,'Empty')
ac(4,'StringLiteral','= %s')
ac(4,'PrimaryName','= %s')

nn('TypeDefinition')
ac(4,[('Identifier'),('TypeSignature'),('TypeInitialisation')],'type %s %s %s')

nn('TypeInitialisation')
ac(4,'TypeExpression','= %s')

nn('PackageDefinition')
ac(4,[('PackageAttributes'),('PackageNameOpt'),('PackageBody')],'%s package %s %s')

nn('PackageAttributes')
ac(4,'','internal',True)
ac(4,'Empty')

nn('PackageNameOpt')
ac(4,'Empty')
ac(4,'PackageName')

nn('PackageName')
ac(4,'Identifier')
ac(4,[('PackageName'),('Identifier')],'%s . %s')

nn('PackageBody')
ac(4,('Block',gl))

nn('UnitDefinition')
ac(4,[('UnitName'),('UnitBody')],'unit %s %s')

nn('UnitName')
ac(4,'Identifier')
ac(4,[('UnitName'),('Identifier')],'%s . %s')

nn('UnitBody')
ac(4,('Block',gl))

## PRAGMAS ##

nn('Pragmas')
ac(4,'Pragma')
ac(4,[('Pragmas'),('Pragma')])

nn('Pragma')
ac(4,[('UsePragma'),('Semicolon',full)])
ac(4,[('ImportPragma'),('Semicolon',full)])

nn('UsePragma')
ac(4,'PragmaItems','use %s')

nn('PragmaItems')
ac(4,'PragmaItem')
ac(4,[('PragmaItems'),('PragmaItem')],'%s , %s')

nn('PragmaItem')
ac(4,('LeftHandSideExpression',allowColon+allowIn),'decimal %s')
ac(4,'PrimaryName','default namespace %s')
ac(4,'PrimaryName','namespace %s')
ac(4,'','standard',True)
ac(4,'','strict',True)
ac(4,[('UnitName'),('StringLiteral')],'unit %s %s')

nn('ImportPragma')
ac(4,'ImportName','import %s')
ac(4,[('Identifier'),('ImportName')],'import %s = %s')

nn('ImportName')
ac(4,'PackageName','%s .  *')
ac(4,[('PackageName'),('Identifier')],'%s.%s')

## BLOCKS AND PROGRAMS ##

nn('Block',t)
ac(3,('Directives',t), '{ %s }')

nn('Program')
ac(3,('Directives',gl))

nn('Empty')
ac(3,'Empty',' ',True)

### END ###