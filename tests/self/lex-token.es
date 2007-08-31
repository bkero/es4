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
    const MinusMinus = Token::Minus - 1
    const Not = Token::MinusMinus - 1
    const NotEqual = Token::Not - 1
    const StrictNotEqual = Token::NotEqual - 1
    const Remainder = Token::StrictNotEqual - 1
    const RemainderAssign = Token::Remainder - 1
    const BitwiseAnd = Token::RemainderAssign - 1
    const LogicalAnd = Token::BitwiseAnd - 1
    const LogicalAndAssign = Token::LogicalAnd - 1
    const BitwiseAndAssign = Token::LogicalAndAssign - 1
    const LeftParen = Token::BitwiseAndAssign - 1
    const RightParen = Token::LeftParen - 1
    const Mult = Token::RightParen - 1
    const MultAssign = Token::Mult - 1
    const Comma = Token::MultAssign  - 1
    const Dot = Token::Comma - 1
    const DoubleDot = Token::Dot - 1
    const TripleDot = Token::DoubleDot - 1
    const LeftDotAngle = Token::TripleDot - 1
    const Div = Token::LeftDotAngle - 1
    const DivAssign = Token::Div - 1
    const Colon = Token::DivAssign - 1
    const DoubleColon = Token::Colon - 1
    const SemiColon = Token::DoubleColon - 1
    const QuestionMark = Token::SemiColon - 1
    const At = Token::QuestionMark - 1
    const LeftBracket = Token::At - 1
    const RightBracket = Token::LeftBracket - 1
    const LogicalXor = Token::RightBracket - 1
    const LogicalXorAssign = Token::LogicalXor - 1
    const LeftBrace = Token::LogicalXorAssign - 1
    const LogicalOr = Token::LeftBrace - 1
    const LogicalOrAssign = Token::LogicalOr - 1
    const BitwiseOr = Token::LogicalOrAssign - 1
    const BitwiseOrAssign = Token::BitwiseOr - 1
    const BitwiseXor = Token::LogicalOrAssign - 1
    const BitwiseXorAssign = Token::BitwiseXor - 1
    const RightBrace = Token::BitwiseXorAssign - 1
    const BitwiseNot = Token::RightBrace - 1
    const Plus = Token::BitwiseNot - 1
    const PlusPlus = Token::Plus - 1
    const PlusAssign = Token::PlusPlus - 1
    const LessThan = Token::PlusAssign - 1
    const LeftShift = Token::LessThan - 1
    const LeftShiftAssign = Token::LeftShift - 1
    const LessThanOrEqual = Token::LeftShiftAssign - 1
    const Assign = Token::LessThanOrEqual - 1
    const MinusAssign = Token::Assign - 1
    const Equal = Token::MinusAssign - 1
    const StrictEqual = Token::Equal - 1
    const GreaterThan = Token::StrictEqual - 1
    const GreaterThanOrEqual = Token::GreaterThan - 1
    const RightShift = Token::GreaterThanOrEqual - 1
    const RightShiftAssign = Token::RightShift - 1
    const UnsignedRightShift = Token::RightShiftAssign - 1
    const UnsignedRightShiftAssign = Token::UnsignedRightShift - 1

    /* reserved identifiers */

    const Break = Token::UnsignedRightShiftAssign - 1
    const Case = Token::Break - 1
    const Catch = Token::Case - 1
    const Class = Token::Catch - 1
    const Continue = Token::Class - 1
    const Default = Token::Continue - 1
    const Delete = Token::Default - 1
    const Do = Token::Delete - 1
    const Else = Token::Do - 1
    const Enum = Token::Else - 1
    const Extends = Token::Enum - 1
    const False = Token::Extends - 1
    const Finally = Token::False - 1
    const For = Token::Finally - 1
    const Function = Token::For - 1
    const If = Token::Function - 1
    const In = Token::If - 1
    const InstanceOf = Token::In - 1
    const New = Token::InstanceOf - 1
    const Null = Token::New - 1
    const Return = Token::Null - 1
    const Super = Token::Return - 1
    const Switch = Token::Super - 1
    const This = Token::Switch - 1
    const Throw = Token::This - 1
    const True = Token::Throw - 1
    const Try = Token::True - 1
    const TypeOf = Token::Try - 1
    const Var = Token::TypeOf - 1
    const Void = Token::Var - 1
    const While = Token::Void - 1
    const With = Token::While - 1

    /* contextually reserved identifiers */

    const Call = Token::With - 1
    const Cast = Token::Call - 1
    const Const = Token::Cast - 1
    const Decimal = Token::Const - 1
    const Double = Token::Decimal - 1
    const Dynamic = Token::Double - 1
    const Each = Token::Dynamic - 1
    const Eval = Token::Each - 1
    const Final = Token::Eval - 1
    const Get = Token::Final - 1
    const Has = Token::Get - 1
    const Implements = Token::Has - 1
    const Import = Token::Implements - 1
    const Int = Token::Import - 1
    const Interface = Token::Int - 1
    const Internal = Token::Interface - 1
    const Intrinsic = Token::Internal - 1
    const Is = Token::Intrinsic - 1
    const Let = Token::Is - 1
    const Namespace = Token::Let - 1
    const Native = Token::Namespace - 1
    const Number = Token::Native - 1
    const Override = Token::Number - 1
    const Package = Token::Override - 1
    const Precision = Token::Package - 1
    const Private = Token::Precision - 1
    const Protected = Token::Private - 1
    const Prototype = Token::Protected - 1
    const Public = Token::Prototype - 1
    const Rounding = Token::Public - 1
    const Standard = Token::Rounding - 1
    const Strict = Token::Standard - 1
    const To = Token::Strict - 1
    const Set = Token::To - 1
    const Static = Token::Set - 1
    const Type = Token::Static - 1
    const UInt = Token::Type - 1
    const Undefined = Token::UInt - 1
    const Unit = Token::Undefined - 1
    const Use = Token::Unit - 1
    const Xml = Token::Use - 1
    const Yield = Token::Xml - 1

    /* literals */

    const AttributeIdentifier = Token::Yield - 1
    const BlockComment = Token::AttributeIdentifier - 1
    const DocComment = Token::BlockComment - 1
    const Eol = Token::DocComment - 1
    const Identifier = Token::Eol - 1

    // The interpretation of these 4 literal types can be done during lexing

    const ExplicitDecimalLiteral = Token::Identifier - 1
    const ExplicitDoubleLiteral = Token::ExplicitDecimalLiteral - 1
    const ExplicitIntLiteral = Token::ExplicitDoubleLiteral - 1
    const ExplicitUIntLiteral = Token::ExplicitIntLiteral - 1

    // The interpretation of these 3 literal types is deferred until defn phase

    const DecimalIntegerLiteral = Token::ExplicitUIntLiteral - 1
    const DecimalLiteral = Token::DecimalIntegerLiteral - 1
    const HexIntegerLiteral = Token::DecimalLiteral - 1

    const RegexpLiteral = Token::HexIntegerLiteral - 1
    const SlashSlashComment = Token::RegexpLiteral - 1
    const StringLiteral = Token::SlashSlashComment - 1
    const Space = Token::StringLiteral - 1
    const XmlLiteral = Token::Space - 1
    const XmlPart = Token::XmlLiteral - 1
    const XmlMarkup = Token::XmlPart - 1
    const XmlText = Token::XmlMarkup - 1
    const XmlTagEndEnd = Token::XmlText - 1
    const XmlTagStartEnd = Token::XmlTagEndEnd - 1

    // meta

    const ERROR = Token::XmlTagStartEnd - 1
    const EOS = Token::ERROR - 1
    const BREAK = Token::EOS - 1
    const lastTokenClass = Token::BREAK

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

    class Tok
    {
        var kind;
        var utf8id;
        function Tok(kind,utf8id)
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
            tokenStore.push(new Tok(kind, text));
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

        var tok : Tok = tokenStore[tid];
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
            var tok : Tok = tokenStore[tid];
            var text = tok.tokenText();
        }
        //print("tokenText: ",tid,", ",text);
        return text;
    }

    function test ()
    {
        print ("testing lex-token.es");
        for( var i = firstTokenClass; i >= lastTokenClass; --i )
            print(i,": ",names[-i])
    }

    //Token::test()
}

