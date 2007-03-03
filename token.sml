(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Token = struct

datatype token = 

    (* punctuators *)

      Minus
    | MinusMinus
    | Not
    | NotEquals
    | StrictNotEquals
    | Modulus
    | ModulusAssign
    | BitwiseAnd
    | LogicalAnd
    | LogicalAndAssign
    | BitwiseAndAssign
    | LeftParen
    | RightParen
    | Mult
    | MultAssign
    | Comma
    | Dot
    | DoubleDot
    | TripleDot
    | LeftDotAngle
    | Div
    | DivAssign
    | Colon
    | DoubleColon
    | SemiColon
    | QuestionMark
    | At
    | LeftBracket
    | RightBracket
    | BitwiseXor
    | BitwiseXorAssign
    | LeftBrace
    | BitwiseOr
    | LogicalOr
    | LogicalOrAssign
    | BitwiseOrAssign
    | RightBrace
    | BitwiseNot
    | Plus
    | PlusPlus
    | PlusAssign
    | LessThan
    | LeftShift
    | LeftShiftAssign
    | LessThanOrEquals
    | Assign
    | MinusAssign
    | Equals
    | StrictEquals
    | GreaterThan
    | GreaterThanOrEquals
    | RightShift
    | RightShiftAssign
    | UnsignedRightShift
    | UnsignedRightShiftAssign

    (* reserved identifiers *)

    | As
    | Break
    | Case
    | Cast
    | Catch
    | Class
    | Const
    | Continue
    | Default
    | Delete
    | Do
    | Else
    | Enum
    | Extends
    | False
    | Finally
    | For
    | Function
    | If
    | Implements
    | Import
    | In
    | InstanceOf
    | Interface
    | Internal
    | Intrinsic
    | Is
    | Let
    | New
    | Null
    | Package
    | Private
    | Protected
    | Public
    | Return
    | Super
    | Switch
    | This
    | Throw
    | To
    | True
    | Try
    | TypeOf
    | Use
    | Var
    | Void
    | While
    | With

    (* contextually reserved identifiers *)

    | Call
    | Construct
    | Debugger
    | Decimal
    | Double
    | Dynamic
    | Each
    | Final
    | Get
    | Goto
    | Include
    | Int
    | Namespace
    | Native
    | Number
    | Override
    | Precision
    | Prototype
    | Rounding
    | Standard
    | Strict
    | UInt
    | Set
    | Static
    | Type
    | Undefined
    | Xml
    | Yield

    (* literals *)

    | AttributeIdentifier
    | BlockComment
    | DocComment
    | Eol
    | Identifier of string
    | NumberLiteral of real (* should be string *)
    | DoubleLiteral of real
    | DecmialLiteral of string
    | IntLiteral of int
    | UIntLiteral of word
    | PackageIdentifier of string
    | RegexpLiteral of string
    | SlashSlashComment
    | StringLiteral of string
    | Whitespace
    | XmlLiteral
    | XmlPart
    | XmlMarkup
    | XmlText
    | XmlTagEndEnd
    | XmlTagStartEnd

    (* meta *)

    | Error
    | LexBreakDiv of { lex_initial: unit -> (token list),
               lex_regexp: unit -> (token list) }
    | LexBreakDivAssign of { lex_initial: unit -> (token list),
                 lex_regexp: unit -> (token list) }
    | LexBreakLessThan of { lex_initial: unit -> (token list),
                lex_xml: unit -> (token list) }
    | Eof

exception TokenError

fun isreserved t = 
    case t of
    ( As
      | Break
      | Case
      | Cast
      | Catch
      | Class
      | Const
      | Continue
      | Default
      | Delete
      | Do
      | Else
      | Enum
      | Extends
      | False
      | Finally
      | For
      | Function
      | If
      | Implements
      | Import
      | In
      | InstanceOf
      | Interface
      | Internal
      | Intrinsic
      | Is
      | Let
      | New
      | Null
      | Package
      | Private
      | Protected
      | Public
      | Return
      | Super
      | Switch
      | This
      | Throw
      | To
      | True
      | Try
      | TypeOf
      | Use
      | Var
      | Void
      | While
      | With ) => true
      | _ => false
         
fun tokenname t =
    case t of
    
    (* punctuators *)
    
    Minus => "-"
      | MinusMinus => "--"
      | Not => "!"
      | NotEquals => "!="
      | StrictNotEquals => "!=="
      | Modulus => "%"
      | ModulusAssign => "%="
      | BitwiseAnd => "&"
      | LogicalAnd => "&&"
      | LogicalAndAssign => "&&="
      | BitwiseAndAssign => "&="
      | LeftParen => "("
      | RightParen => ")"
      | Mult => "*"
      | MultAssign => "*="
      | Comma => ","
      | Dot => "."
      | DoubleDot => ".."
      | TripleDot => "..."
      | LeftDotAngle => ".<"
      | Div => "/"
      | DivAssign => "/="
      | Colon => ":"
      | DoubleColon => "::"
      | SemiColon => ";"
      | QuestionMark => "?"
      | At => "@"
      | LeftBracket => "["
      | RightBracket => "]"
      | BitwiseXor => "^"
      | BitwiseXorAssign => "^="
      | LeftBrace => "{"
      | BitwiseOr => "|"
      | LogicalOr => "||"
      | LogicalOrAssign => "||="
      | BitwiseOrAssign => "|="
      | RightBrace => "}"
      | BitwiseNot => "~"
      | Plus => "+"
      | PlusPlus => "++"
      | PlusAssign => "+="
      | LessThan => "<"
      | LeftShift => "<<"
      | LeftShiftAssign => "<<="
      | LessThanOrEquals => "<="
      | Assign => "="
      | MinusAssign => "-="
      | Equals => "=="
      | StrictEquals => "==="
      | GreaterThan => ">"
      | GreaterThanOrEquals => ">="
      | RightShift => ">>"
      | RightShiftAssign => ">>="
      | UnsignedRightShift => ">>>"
      | UnsignedRightShiftAssign => ">>>="

      (* reserved identifiers *)

      | As => "as"
      | Break => "break"
      | Case => "case"
      | Cast => "cast"
      | Catch => "catch"
      | Class => "class"
      | Const => "const"
      | Continue => "continue"
      | Default => "default"
      | Delete => "delete"
      | Do => "do"
      | Else => "else"
      | Enum => "enum"
      | Extends => "extends"
      | False => "false"
      | Finally => "finally"
      | For => "for"
      | Function => "function"
      | If => "if"
      | Implements => "implements"
      | Import => "import"
      | In => "in"
      | InstanceOf => "instanceof"
      | Interface => "interface"
      | Internal => "internal"
      | Intrinsic => "intrinsic"
      | Is => "is"
      | Let => "let"
      | New => "new"
      | Null => "null"
      | Package => "package"
      | Private => "private"
      | Protected => "protected"
      | Public => "public"
      | Return => "return"
      | Super => "super"
      | Switch => "switch"
      | This => "this"
      | Throw => "throw"
      | To => "to"
      | True => "true"
      | Try => "try"
      | TypeOf => "typeof"
      | Use => "use"
      | Var => "var"
      | Void => "void"
      | While => "while"
      | With => "with"

      (* contextually reserved identifiers *)

      | Call => "call"
      | Construct => "construct"
      | Debugger => "debugger"
      | Decimal => "decimal"
      | Double => "double"
      | Dynamic => "dynamic"
      | Each => "each"
      | Final => "final"
      | Get => "get"
      | Goto => "goto"
      | Include => "include"
      | Int => "int"
      | Namespace => "namespace"
      | Native => "native"
      | Number => "number"
      | Override => "override"
      | Precision => "precision"
      | Prototype => "prototype"
      | Rounding => "rounding"
      | Standard => "standard"
      | Strict => "strict"
      | Set => "set"
      | Static => "static"
      | Type => "type"
      | UInt => "uint"
      | Undefined => "undefined"
      | Xml => "xml"
      | Yield => "yield"

      (* literals *)

      | AttributeIdentifier => "@id"
      | BlockComment => ""
      | DocComment => ""
      | Eol => "eol"
      | Identifier x => "identifier("^x^")"
      | NumberLiteral x => Real.toString(x)
      | DoubleLiteral x => Real.toString(x)
      | DecmialLiteral x => x
      | IntLiteral x => Int.toString(x)
      | UIntLiteral x => Word.toString(x)
      | PackageIdentifier x => "packageidentifier("^x^")"
      | RegexpLiteral x => "regexp("^x^")"
      | SlashSlashComment => ""
      | StringLiteral x => "string("^x^")"
      | Whitespace => "<ws>"
      | XmlLiteral => "xmlliteral()"
      | XmlPart => "xmlpart()"
      | XmlMarkup => "xmlmarkup()"
      | XmlText => "xmltext()"
      | XmlTagEndEnd => "xmltagendend()"
      | XmlTagStartEnd => "xmltagstartend()"

      (* meta tokens *)

      | Error => "error"
      | LexBreakDiv x => "lexbreak_div"
      | LexBreakDivAssign x => "lexbreak_divassign"
      | LexBreakLessThan x => "lexbreak_lessthan"
      | Eof => "eof"
end
