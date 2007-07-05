(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
structure Token = struct

datatype TOKEN =

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

    | Break
    | Case
    | Catch
    | Class
    | Continue
    | Debugger
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
    | Goto
    | If
    | In
    | InstanceOf
    | New
    | Null
    | Return
    | Super
    | Switch
    | This
    | Throw
    | True
    | Try
    | TypeOf
    | Var
    | Void
    | While
    | With

    (* contextually reserved identifiers *)

    | Call
    | Cast
    | Const
    | Decimal
    | Double
    | Dynamic
    | Each
    | Eval
    | Final
    | Get
    | Has
    | Implements
    | Import
    | Int
    | Interface
    | Internal
    | Intrinsic
    | Is
    | Let
    | Namespace
    | Native
    | Number
    | Override
    | Package
    | Precision
    | Private
    | Protected
    | Prototype
    | Public
    | Rounding
    | Set
    | Standard
    | Static
    | Strict
    | To
    | Type
    | UInt
    | Undefined
    | Use
    | Xml
    | Yield

    (* literals *)

    | Identifier of Ustring.STRING

    (* The interpretation of these 4 literal types can be done during lexing. *)
    | ExplicitDecimalLiteral of Decimal.DEC
    | ExplicitDoubleLiteral of Real64.real
    | ExplicitIntLiteral of Int32.int
    | ExplicitUIntLiteral of Word32.word

    (* The interpretation of these 3 literal types is deferred until defn phase. *)
    | DecimalIntegerLiteral of string
    | DecimalLiteral of string
    | HexIntegerLiteral of string

    | PackageIdentifier of Ustring.STRING
    | RegexpLiteral of Ustring.STRING
    | StringLiteral of Ustring.STRING
    | XmlLiteral
    | XmlPart
    | XmlMarkup
    | XmlText
    | XmlTagEndEnd
    | XmlTagStartEnd

    (* meta *)

    | LexBreakDiv of { lex_initial: unit -> ((TOKEN * Ast.LOC) list),
                       lex_regexp: unit -> ((TOKEN * Ast.LOC) list) }
    | LexBreakLessThan of { lex_initial: unit -> ((TOKEN * Ast.LOC) list),
                            lex_xml: unit -> ((TOKEN * Ast.LOC) list) }
    | Eof

exception TokenError

fun isreserved (t,_) =
    case t of
        Break => true
      | Case => true
      | Catch => true
      | Class => true
      | Continue => true
      | Debugger => true
      | Default => true
      | Delete => true
      | Do => true
      | Else => true
      | Enum => true
      | Extends => true
      | False => true
      | Finally => true
      | For => true
      | Function => true
      | If => true
      | In => true
      | InstanceOf => true
      | New => true
      | Null => true
      | Return => true
      | Super => true
      | Switch => true
      | This => true
      | Throw => true
      | True => true
      | Try => true
      | TypeOf => true
      | Var => true
      | Void => true
      | While => true
      | With => true
      | _ => false

fun tokenname (t,_) =
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

      | Break => "break"
      | Case => "case"
      | Catch => "catch"
      | Class => "class"
      | Continue => "continue"
      | Debugger => "debugger"
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
      | Goto => "goto"
      | If => "if"
      | In => "in"
      | InstanceOf => "instanceof"
      | New => "new"
      | Null => "null"
      | Return => "return"
      | Super => "super"
      | Switch => "switch"
      | This => "this"
      | Throw => "throw"
      | True => "true"
      | Try => "try"
      | TypeOf => "typeof"
      | Var => "var"
      | Void => "void"
      | While => "while"
      | With => "with"

      (* contextually reserved identifiers *)

      | Call => "call"
      | Cast => "cast"
      | Const => "const"
      | Decimal => "decimal"
      | Double => "double"
      | Dynamic => "dynamic"
      | Each => "each"
      | Eval => "eval"
      | Final => "final"
      | Get => "get"
      | Has => "has"
      | Implements => "implements"
      | Import => "import"
      | Int => "int"
      | Interface => "interface"
      | Internal => "internal"
      | Intrinsic => "intrinsic"
      | Is => "is"
      | Let => "let"
      | Namespace => "namespace"
      | Native => "native"
      | Number => "Number"
      | Override => "override"
      | Package => "package"
      | Precision => "precision"
      | Private => "private"
      | Protected => "protected"
      | Prototype => "prototype"
      | Public => "public"
      | Rounding => "rounding"
      | Set => "set"
      | Standard => "standard"
      | Static => "static"
      | Strict => "strict"
      | To => "to"
      | Type => "type"
      | UInt => "uint"
      | Undefined => "undefined"
      | Use => "use"
      | Xml => "xml"
      | Yield => "yield"

      (* literals *)

      | Identifier x => "identifier("^(Ustring.toAscii x)^")"

      | DecimalIntegerLiteral x => x
      | DecimalLiteral        x => x
      | HexIntegerLiteral     x => x

      | ExplicitDecimalLiteral x => Decimal.toString(x) ^ "m"
      | ExplicitDoubleLiteral x => Real64.toString(x) ^ "d"
      | ExplicitIntLiteral x => Int32.toString(x) ^ "i"
      | ExplicitUIntLiteral x => LargeInt.toString (Word32.toLargeInt(x)) ^ "u"

      | PackageIdentifier x => "packageidentifier("^(Ustring.toAscii x)^")"
      | RegexpLiteral x => "regexp("^(Ustring.toAscii x)^")"
      | StringLiteral x => "string("^(Ustring.toAscii x)^")"
      | XmlLiteral => "xmlliteral()"
      | XmlPart => "xmlpart()"
      | XmlMarkup => "xmlmarkup()"
      | XmlText => "xmltext()"
      | XmlTagEndEnd => "xmltagendend()"
      | XmlTagStartEnd => "xmltagstartend()"

      (* meta tokens *)

      | LexBreakDiv x => "lexbreak_div"
      | LexBreakLessThan x => "lexbreak_lessthan"
      | Eof => "eof"
end
