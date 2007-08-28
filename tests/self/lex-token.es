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

use namespace intrinsic;

namespace Token

{
    use default namespace Token

    const firstTokenClass = -1
    const Minus = firstTokenClass
    const MinusMinus = Minus - 1
    const Not = MinusMinus - 1
    const NotEqual = Not - 1
    const StrictNotEqual = NotEqual - 1
    const Remainder = StrictNotEqual - 1
    const RemainderAssign = Remainder - 1
    const BitwiseAnd = RemainderAssign - 1
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
    const LogicalXor = RightBracket - 1
    const LogicalXorAssign = LogicalXor - 1
    const LeftBrace = LogicalXorAssign - 1
    const LogicalOr = LeftBrace - 1
    const LogicalOrAssign = LogicalOr - 1
    const BitwiseOr = LogicalOrAssign - 1
    const BitwiseOrAssign = BitwiseOr - 1
    const BitwiseXor = LogicalOrAssign - 1
    const BitwiseXorAssign = BitwiseXor - 1
    const RightBrace = BitwiseXorAssign - 1
    const BitwiseNot = RightBrace - 1
    const Plus = BitwiseNot - 1
    const PlusPlus = Plus - 1
    const PlusAssign = PlusPlus - 1
    const LessThan = PlusAssign - 1
    const LeftShift = LessThan - 1
    const LeftShiftAssign = LeftShift - 1
    const LessThanOrEqual = LeftShiftAssign - 1
    const Assign = LessThanOrEqual - 1
    const MinusAssign = Assign - 1
    const Equal = MinusAssign - 1
    const StrictEqual = Equal - 1
    const GreaterThan = StrictEqual - 1
    const GreaterThanOrEqual = GreaterThan - 1
    const RightShift = GreaterThanOrEqual - 1
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
    const In = If - 1
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
    const Decimal = Const - 1
    const Double = Decimal - 1
    const Dynamic = Double - 1
    const Each = Dynamic - 1
    const Eval = Each - 1
    const Final = Eval - 1
    const Get = Final - 1
    const Has = Get - 1
    const Implements = Has - 1
    const Import = Implements - 1
    const Int = Import - 1
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
    const Unit = Undefined - 1
    const Use = Unit - 1
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
    const Space = StringLiteral - 1
    const XmlLiteral = Space - 1
    const XmlPart = XmlLiteral - 1
    const XmlMarkup = XmlPart - 1
    const XmlText = XmlMarkup - 1
    const XmlTagEndEnd = XmlText - 1
    const XmlTagStartEnd = XmlTagEndEnd - 1

    // meta

    const ERROR = XmlTagStartEnd - 1
    const EOS = ERROR - 1
    const BREAK = EOS - 1
    const lastTokenClass = BREAK

    const names = [
        "<unused index>",
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
        "break",
        "case",
        "catch",
        "class",
        "continue",
        "default",
        "delete",
        "do",
        "else",
        "enum",
        "extends",
        "false",
        "finally",
        "for",
        "function",
        "if",
        "in",
        "instanceof",
        "new",
        "null",
        "return",
        "super",
        "switch",
        "this",
        "throw",
        "true",
        "try",
        "typeof",
        "var",
        "void",
        "while",
        "with",

        "call",
        "cast",
        "const",
        "decimal",
        "double",
        "dynamic",
        "each",
        "eval",
        "final",
        "get",
        "has",
        "implements",
        "import",
        "int",
        "interface",
        "internal",
        "intrinsic",
        "is",
        "let",
        "namespace",
        "native",
        "Number",
        "override",
        "package",
        "precision",
        "private",
        "protected",
        "prototype",
        "public",
        "rounding",
        "standard",
        "strict",
        "to",
        "set",
        "static",
        "type",
        "uint",
        "undefined",
        "unit",
        "use",
        "xml",
        "yield",

        "attributeidentifier",
        "blockcomment",
        "doccomment",
        "eol",
        "identifier",
        "explicitdecimalliteral",
        "explicitdoubleliteral",
        "explicitintliteral",
        "explicituintliteral",
        "decimalintegerliteral",
        "decimalliteral",
        "hexintegerliteral",
        "regexpliteral",
        "linecomment",
        "stringliteral",
        "space",
        "xmlliteral",
        "xmlpart",
        "xmlmarkup",
        "xmltext",
        "xmltagendend",
        "xmltagstartend",

        "ERROR",
        "EOS",
        "BREAK"
    ]

    class Token
    {
        var kind;
        var utf8id;
        function Token(kind,utf8id)
            : kind = kind
            , utf8id = utf8id
        {
        }

        function tokenText () : String
        {
            if (kind===StringLiteral) {
                return this.utf8id.slice(1,this.utf8id.length);
            }
            return this.utf8id;
        }

        function tokenKind () : int
        {
            return this.kind;
        }
    }

    const tokenStore = new Array;

    function maybeReservedIdentifier (lexeme:String) : int
    {
        // ("maybeReservedIdentifier lexeme=",lexeme);
        switch (lexeme) {

        // ContextuallyReservedIdentifiers

        case "break": return Break;
        case "case": return Case;
        case "catch": return Catch;
        case "class": return Class;
        case "continue": return Continue;
        case "default": return Default;
        case "delete": return Delete;
        case "do": return Do;
        case "else": return Else;
        case "enum": return Enum;
        case "extends": return Extends;
        case "false": return False;
        case "finally": return Finally;
        case "for": return For;
        case "function": return Function;
        case "if": return If;
        case "in": return In;
        case "instanceof": return InstanceOf;
        case "new": return New;
        case "null": return Null;
        case "return": return Return;
        case "super": return Super;
        case "switch": return Switch;
        case "this": return This;
        case "throw": return Throw;
        case "true": return True;
        case "try": return Try;
        case "typeof": return TypeOf;
        case "var": return Var;
        case "void": return Void;
        case "while": return While;
        case "with": return With;

        // ContextuallyReservedIdentifiers

        case "call": return Call;
        case "cast": return Cast;
        case "const": return Const;
        case "decimal": return Decimal;
        case "double": return Double;
        case "dynamic": return Dynamic;
        case "each": return Each;
        case "eval": return Eval;
        case "final": return Final;
        case "get": return Get;
        case "has": return Has;
        case "implements": return Implements;
        case "import": return Import;
        case "int": return Int;
        case "interface" : return Interface;
        case "internal": return Internal;
        case "intrinsic": return Intrinsic;
        case "is": return Is;
        case "let": return Let;
        case "namespace": return Namespace;
        case "native": return Native;
        case "Number": return Number;
        case "override": return Override;
        case "package": return Package;
        case "precision": return Precision;
        case "private": return Private;
        case "protected": return Protected;
        case "prototype": return Prototype;
        case "public": return Public;
        case "rounding": return Rounding;
        case "standard": return Standard;
        case "strict": return Strict;
        case "to": return To;
        case "set": return Set;
        case "static": return Static;
        case "to": return To;
        case "type": return Type;
        case "uint": return UInt;
        case "undefined": return Undefined;
        case "use": return Use;
        case "unit": return Unit;
        case "xml": return Xml;
        case "yield": return Yield;
        default: return makeInstance (Identifier,lexeme);
        }
    }

    function makeInstance(kind:int, text:String) : int
    {
        function find() {
            for ( var i=0 ; i < len ; i++ ) {
                if (tokenStore[i].kind === kind &&
                    tokenStore[i].utf8id == text) {
                    return i;
                }
            }
            return len;
        }

        var len = tokenStore.length;
        var tid = find (kind,text);
        if (tid === len) 
        {
            tokenStore.push(new Token(kind, text));
        }
        return tid;
    }

    function tokenKind (tid : int) : int
    {
        // if the token id is negative, it is a token_class

        //print("tid=",tid);
        if (tid < 0)
        {
           return tid;
        }

        // otherwise, get instance data from the instance vector.

        var tok : Token = tokenStore[tid];
        return tok.kind;
    }

    function tokenText ( tid : int ) : String
    {
        if (tid < 0) {
            // if the token id is negative, it is a token_class.
            var text = names[-tid];
        }
        else {
            // otherwise, get instance data from the instance vector
            var tok : Token = tokenStore[tid];
            var text = tok.tokenText();
        }
        //print("tokenText: ",tid,", ",text);
        return text;
    }

    function test ()
    {
        for( let i = firstTokenClass; i >= lastTokenClass; --i )
            print(i,": ",names[-i])
    }

    //test()
}

