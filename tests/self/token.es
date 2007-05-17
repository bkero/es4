/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is [Open Source Virtual Machine.].
 *
 * The Initial Developer of the Original Code is
 * Adobe System Incorporated.
 * Portions created by the Initial Developer are Copyright (C) 2004-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Adobe AS3 Team
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */


namespace Token = "Token"

{
    use default namespace Token

    const FirstToken = 0
    const Eof = FirstToken -1
    const Minus = Eof - 1
    const MinusMinus = Minus - 1
    const Not = MinusMinus - 1
    const NotEquals = Not - 1
    const StrictNotEquals = NotEquals - 1
    const Modulus = StrictNotEquals - 1
    const ModulusAssign = Modulus - 1
    const BitwiseAnd = ModulusAssign - 1
    const LogicalAnd = BitwiseAnd - 1
    const LogicalAndAssign = LogicalAnd - 1
    const BitwiseAndAssign = LogicalAndAssign - 1
    const LeftParen = BitwiseAndAssign - 1
    const RightParen = LeftParen - 1
    const Mult = RightParen - 1
    const MultAssign = Mult - 1
    const Comma = MultAssign  - 1
    const Dot = Comma - 1
    const DoubleDot = Dot - 1
    const TripleDot = DoubleDot - 1
    const LeftDotAngle = TripleDot - 1
    const Div = LeftDotAngle - 1
    const DivAssign = Div - 1
    const Colon = DivAssign - 1
    const DoubleColon = Colon - 1
    const SemiColon = DoubleColon - 1
    const QuestionMark = SemiColon - 1
    const At = QuestionMark - 1
    const LeftBracket = At - 1
    const RightBracket = LeftBracket - 1
    const BitwiseXor = RightBracket - 1
    const BitwiseXorAssign = BitwiseXor - 1
    const LeftBrace = BitwiseXorAssign - 1
    const BitwiseOr = LeftBrace - 1
    const LogicalOr = BitwiseOr - 1
    const LogicalOrAssign = LogicalOr - 1
    const BitwiseOrAssign = LogicalOrAssign - 1
    const RightBrace = BitwiseOrAssign - 1
    const BitwiseNot = RightBrace - 1
    const Plus = BitwiseNot - 1
    const PlusPlus = Plus - 1
    const PlusAssign = PlusPlus - 1
    const LessThan = PlusAssign - 1
    const LeftShift = LessThan - 1
    const LeftShiftAssign = LeftShift - 1
    const LessThanOrEquals = LeftShiftAssign - 1
    const Assign = LessThanOrEquals - 1
    const MinusAssign = Assign - 1
    const Equals = MinusAssign - 1
    const StrictEquals = Equals - 1
    const GreaterThan = StrictEquals - 1
    const GreaterThanOrEquals = GreaterThan - 1
    const RightShift = GreaterThanOrEquals - 1
    const RightShiftAssign = RightShift - 1
    const UnsignedRightShift = RightShiftAssign - 1
    const UnsignedRightShiftAssign = UnsignedRightShift - 1

    /* reserved identifiers */

    const Break = UnsignedRightShiftAssign - 1
    const Case = Break - 1
    const Catch = Case - 1
    const Class = Catch - 1
    const Continue = Class - 1
    const Default = Continue - 1
    const Delete = Default - 1
    const Do = Delete - 1
    const Else = Do - 1
    const Enum = Else - 1
    const Extends = Enum - 1
    const False = Extends - 1
    const Finally = False - 1
    const For = Finally - 1
    const Function = For - 1
    const If = Function - 1
    const Import = If - 1
    const In = Import - 1
    const InstanceOf = In - 1
    const New = InstanceOf - 1
    const Null = New - 1
    const Return = Null - 1
    const Super = Return - 1
    const Switch = Super - 1
    const This = Switch - 1
    const Throw = This - 1
    const True = Throw - 1
    const Try = True - 1
    const TypeOf = Try - 1
    const Var = TypeOf - 1
    const Void = Var - 1
    const While = Void - 1
    const With = While - 1

        /* contextually reserved identifiers */

    const Call = With - 1
    const Cast = Call - 1
    const Const = Cast - 1
    const Debugger = Const - 1
    const Decimal = Debugger - 1
    const Double = Decimal - 1
    const Dynamic = Double - 1
    const Each = Dynamic - 1
    const Final = Each - 1
    const Get = Final - 1
    const Goto = Get - 1
    const Has = Goto - 1
    const Implements = Has - 1
    const Int = Implements - 1
    const Interface = Int - 1
    const Internal = Interface - 1
    const Intrinsic = Internal - 1
    const Is = Intrinsic - 1
    const Let = Is - 1
    const Namespace = Let - 1
    const Native = Namespace - 1
    const Number = Native - 1
    const Override = Number - 1
    const Package = Override - 1
    const Precision = Package - 1
    const Private = Precision - 1
    const Protected = Private - 1
    const Prototype = Protected - 1
    const Public = Prototype - 1
    const Rounding = Public - 1
    const Standard = Rounding - 1
    const Strict = Standard - 1
    const To = Strict - 1
    const Set = To - 1
    const Static = Set - 1
    const Type = Static - 1
    const UInt = Type - 1
    const Undefined = UInt - 1
    const Use = Undefined - 1
    const Xml = Use - 1
    const Yield = Xml - 1

        /* literals */

    const AttributeIdentifier = Yield - 1
    const BlockComment = AttributeIdentifier - 1
    const DocComment = BlockComment - 1
    const Eol = DocComment - 1
    const Identifier = Eol - 1

    // The interpretation of these 4 literal types can be done during lexing

    const ExplicitDecimalLiteral = Identifier - 1
    const ExplicitDoubleLiteral = ExplicitDecimalLiteral - 1
    const ExplicitIntLiteral = ExplicitDoubleLiteral - 1
    const ExplicitUIntLiteral = ExplicitIntLiteral - 1

    // The interpretation of these 3 literal types is deferred until defn phase

    const DecimalIntegerLiteral = ExplicitUIntLiteral - 1
    const DecimalLiteral = DecimalIntegerLiteral - 1
    const HexIntegerLiteral = DecimalLiteral - 1

    const RegexpLiteral = HexIntegerLiteral - 1
    const SlashSlashComment = RegexpLiteral - 1
    const StringLiteral = SlashSlashComment - 1
    const Whitespace = StringLiteral - 1
    const XmlLiteral = Whitespace - 1
    const XmlPart = XmlLiteral - 1
    const XmlMarkup = XmlPart - 1
    const XmlText = XmlMarkup - 1
    const XmlTagEndEnd = XmlText - 1
    const XmlTagStartEnd = XmlTagEndEnd - 1

    // meta

    const Error = XmlTagStartEnd - 1
    const Eof = Error - 1
    const LexBreakDiv = Eof - 1 
                            // of { lex_initial: unit -> ((TOKEN * Ast.POS) list),
                            // lex_regexp: unit -> ((TOKEN * Ast.POS) list) }
    const LexBreakDivAssign  = LexBreakDiv - 1 
                            // of { lex_initial: unit -> ((TOKEN * Ast.POS) list),
                            // lex_regexp: unit -> ((TOKEN * Ast.POS) list) }
    const LexBreakLessThan = LexBreakDivAssign - 1 
                            // of { lex_initial: unit -> ((TOKEN * Ast.POS) list),
                            //lex_xml: unit -> ((TOKEN * Ast.POS) list) }
    const LastToken = LexBreakLessThan

    const names = [
        "<unused index>", 
        "end of program",
        "minus",
        "minusminus",
        "not",
        "notequals",
        "strictnotequals",
        "modulus",
        "modulusassign",
        "bitwiseand",
        "logicaland",
        "logicalandassign",
        "bitwiseandassign",
        "leftparen",
        "rightparen",
        "mult",
        "multassign",
        "comma",
        "dot",
        "doubledot",
        "tripledot",
        "leftdotangle",
        "div",
        "divassign",
        "colon",
        "doublecolon",
        "semicolon",
        "questionmark",
        "at",
        "leftbracket",
        "rightbracket",
        "bitwisexor",
        "logicalxor",
        "logicalxorassign",
        "bitwisexorassign",
        "leftbrace",
        "bitwiseor",
        "logicalor",
        "logicalorassign",
        "bitwiseorassign",
        "rightbrace",
        "bitwisenot",
        "plus",
        "plusplus",
        "plusassign",
        "lessthan",
        "leftshift",
        "leftshiftassign",
        "lessthanorequals",
        "assign",
        "minusassign",
        "equals",
        "strictequals",
        "greaterthan",
        "greaterthanorequals",
        "rightshift",
        "rightshiftassign",
        "unsignedrightshift",
        "unsignedrightshiftassign",
        "as",
        "break",
        "call",
        "case",
        "cast",
        "catch",
        "class",
        "const",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "dynamic",
        "each",
        "else",
        "enum",
        "extends",
        "false",
        "final",
        "finally",
        "for",
        "function",
        "get",
        "goto",
        "if",
        "implements",
        "import",
        "in",
        "include",
        "instanceof",
        "interface",
        "internal",
        "intrinsic",
        "is",
        "let",
        "namespace",
        "native",
        "new",
        "null",
        "override",
        "package",
        "private",
        "protected",
        "prototype",
        "public",
        "return",
        "set",
        "static",
        "super",
        "switch",
        "this",
        "throw",
        "to",
        "true",
        "try",
        "type",
        "typeof",
        "use",
        "var",
        "void",
        "while",
        "with",
        "xml",
        "yield",
        "identifier",
        "numberliteral",
        "regexpliteral",
        "stringliteral",
        "xmlliteral",
        "xmlpart",
        "xmlmarkup",
        "xmltext",
        "xmltagendend",
        "xmltagstartend",
        "attributeidentifier",
        "docComment",
        "blockComment",
        "slashslashcomment",
        
        "<newline>",
        "<ws>",
        "<empty>",
        "<error>",
        "abbrev_mode",
        "full_mode"
    ]
    
    class Token 
    {
        var kind
        var utf8id
        function Token(kind,utf8id)
        {
            this.kind = kind
            this.utf8id = utf8id
        }
    }

    function test () 
    {
        for( let i = FirstToken; i >= LastToken; --i )
            print(i,": ",names[-i])
    }

}

Token::test()
