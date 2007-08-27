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

/*
    TODO

    x lex break
    x regexp literals
    x punctuators
    x string literals
    x comments
    x reserved words
    x escapes
    x number literals
*/

use namespace intrinsic;

namespace Char

{
    use default namespace Char;
    const EOS = 0;
    const a = "a".charCodeAt(0);
    const b = "b".charCodeAt(0);
    const c = "c".charCodeAt(0);
    const d = "d".charCodeAt(0);
    const e = "e".charCodeAt(0);
    const f = "f".charCodeAt(0);
    const g = "g".charCodeAt(0);
    const h = "h".charCodeAt(0);
    const i = "i".charCodeAt(0);
    const j = "j".charCodeAt(0);
    const k = "k".charCodeAt(0);
    const l = "l".charCodeAt(0);
    const m = "m".charCodeAt(0);
    const n = "n".charCodeAt(0);
    const o = "o".charCodeAt(0);
    const p = "p".charCodeAt(0);
    const q = "q".charCodeAt(0);
    const r = "r".charCodeAt(0);
    const s = "s".charCodeAt(0);
    const t = "t".charCodeAt(0);
    const u = "u".charCodeAt(0);
    const v = "v".charCodeAt(0);
    const w = "w".charCodeAt(0);
    const x = "x".charCodeAt(0);
    const y = "y".charCodeAt(0);
    const z = "z".charCodeAt(0);
    const A = "A".charCodeAt(0);
    const B = "B".charCodeAt(0);
    const C = "C".charCodeAt(0);
    const D = "D".charCodeAt(0);
    const E = "E".charCodeAt(0);
    const F = "F".charCodeAt(0);
    const G = "G".charCodeAt(0);
    const H = "H".charCodeAt(0);
    const I = "I".charCodeAt(0);
    const J = "J".charCodeAt(0);
    const K = "K".charCodeAt(0);
    const L = "L".charCodeAt(0);
    const M = "M".charCodeAt(0);
    const N = "N".charCodeAt(0);
    const O = "O".charCodeAt(0);
    const P = "P".charCodeAt(0);
    const Q = "Q".charCodeAt(0);
    const R = "R".charCodeAt(0);
    const S = "S".charCodeAt(0);
    const T = "T".charCodeAt(0);
    const U = "U".charCodeAt(0);
    const V = "V".charCodeAt(0);
    const W = "W".charCodeAt(0);
    const X = "X".charCodeAt(0);
    const Y = "Y".charCodeAt(0);
    const Z = "Z".charCodeAt(0);
    const Zero = "0".charCodeAt(0);
    const One = "1".charCodeAt(0);
    const Two = "2".charCodeAt(0);
    const Three = "3".charCodeAt(0);
    const Four = "4".charCodeAt(0);
    const Five = "5".charCodeAt(0);
    const Six = "6".charCodeAt(0);
    const Seven = "7".charCodeAt(0);
    const Eight = "8".charCodeAt(0);
    const Nine = "9".charCodeAt(0);
    const Dot = ".".charCodeAt(0);
    const Bang = "!".charCodeAt(0);
    const Equal = "=".charCodeAt(0);
    const Percent = "%".charCodeAt(0);
    const Ampersand = "&".charCodeAt(0);
    const Asterisk = "*".charCodeAt(0);
    const Plus = "+".charCodeAt(0);
    const Dash = "-".charCodeAt(0);
    const Slash = "/".charCodeAt(0);
    const BackSlash = "\\".charCodeAt(0);
    const Comma = ",".charCodeAt(0);
    const Colon = ":".charCodeAt(0);
    const Semicolon = ";".charCodeAt(0);
    const LeftAngle = "<".charCodeAt(0);
    const RightAngle = ">".charCodeAt(0);
    const Caret = "^".charCodeAt(0);
    const Bar = "|".charCodeAt(0);
    const QuestionMark = "?".charCodeAt(0);
    const LeftParen = "(".charCodeAt(0);
    const RightParen = ")".charCodeAt(0);
    const LeftBrace = "{".charCodeAt(0);
    const RightBrace = "}".charCodeAt(0);
    const LeftBracket = "[".charCodeAt(0);
    const RightBracket = "]".charCodeAt(0);
    const Tilde = "~".charCodeAt(0);
    const At = "@".charCodeAt(0);
    const SingleQuote = "'".charCodeAt(0);
    const DoubleQuote = "\"".charCodeAt(0);
    const Space = " ".charCodeAt(0);
    const Tab = "\t".charCodeAt(0);
    const Newline = "\n".charCodeAt(0);

    function fromOctal (str)
	: int
    {
	return parseInt (str);
    }

    function fromHex (str)
	: int
    {
	return parseInt (str);
    }
}


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

}

Token::test()

namespace Lexer

{
    use default namespace Lexer;

    class Scanner
    {
        private var src : String;
        private var origin : String;
        private var curIndex : int;
        private var markIndex : int;
        private var lastMarkIndex : int;
        private var colCoord : int;
        private var lnCoord : int;

        public function Scanner (src:String, origin:String)
            : src = src
            , origin = origin
            , curIndex = 0
            , markIndex = 0
            , lastMarkIndex = 0
            , colCoord = 0
            , lnCoord = 0
        {
            print("scanning: ",src);
        }

        public function next ()
            : String
        {
            if (curIndex == src.length)
            {
                curIndex++;
                return Char::EOS;
            }
            else
            {
                return src.charCodeAt(curIndex++);
            }
        }

        public function lexeme()
            : String
        {
            return src.slice (markIndex,curIndex)
        }

        public function retract()
            : void
        {
            curIndex--;
            //print("retract cur=",curIndex);
        }

        private function mark ()
            : void
        {
            markIndex = curIndex;
	    //print("mark mark=",markIndex);
        }

        public function tokenList (lexPrefix)
            //            : [[int],[[int,int]]]
        {
            function pushToken (token)
            {
                if (token == Token::Eol) {
                    lnCoord++;
                    colCoord = 0;
                }
                else {
                    print ("token ", token);
                    //print ("token ", token, " \t", Token::tokenText(token));
                    colCoord += markIndex - lastMarkIndex;
                    coordList.push ([lnCoord,colCoord]);
                    tokenList.push (token);
                    lastMarkIndex = markIndex;
                }
            }

            var tokenList = new Array;
            var coordList = new Array;

            let token = lexPrefix ();
            pushToken (token);

            while (token != Token::BREAK &&
                   token != Token::EOS &&
                   token != Token::ERROR)
            {
                token = start ();
                pushToken (token);
            }

            //print("tokenList = ",tokenList);
            //print("coordList = ",coordList);
            return [tokenList,coordList];
        }

        public function regexp ()
        {
            let c : int = next ();
            switch (c)
            {
            case Char::Slash :
                return regexpFlags ();
	        case Char::EOS :
                throw "unexpected end of program in regexp literal";
            default:
                return regexp ();
            }
        }

        public function regexpFlags ()
        {
            let c : int = next ();
            if (Unicode.isIdentifierPart (String.fromCharCode(c))) {
                return regexpFlags ();
            }
            else {
                retract ();
                return Token::makeInstance (Token::RegexpLiteral,lexeme());
            }
        }

        public function start ()
            : int
        {
            var c : int;
            while (true)
            {
                mark();
                c = next();
                //print("c[",curIndex-1,"]=",String.fromCharCode(c));
                switch (c)
                {
                case 0xffffffef: return utf8sig ();
                case Char::EOS: return Token::EOS;
                case Char::Slash: return slash ();
                case Char::Newline: return Token::Eol;
                case Char::Space: return start ();
                case Char::Tab: return start ();
                case Char::LeftParen: return Token::LeftParen;
                case Char::RightParen: return Token::RightParen;
                case Char::Comma: return Token::Comma;
                case Char::Semicolon: return Token::SemiColon;
                case Char::QuestionMark: return Token::QuestionMark;
                case Char::LeftBracket: return Token::LeftBracket;
                case Char::RightBracket: return Token::RightBracket;
                case Char::LeftBrace: return Token::LeftBrace;
                case Char::RightBrace: return Token::RightBrace;
                case Char::Tilde: return Token::BitwiseNot;
                case Char::At: return Token::At;
                case Char::SingleQuote: return stringLiteral (c);
                case Char::DoubleQuote: return stringLiteral (c);
                case Char::Dot: return dot ();
                case Char::Dash: return minus ();
                case Char::Bang: return not ();
                case Char::Percent: return remainder ();
                case Char::Ampersand: return and ();
                case Char::Asterisk: return mult ();
                case Char::Colon: return colon ();
                case Char::Caret: return bitwiseXor ();
                case Char::Bar: return bitwiseOr ();
                case Char::Plus: return plus ();
                case Char::LeftAngle: return leftAngle ();
                case Char::Equal: return equal ();
                case Char::RightAngle: return rightAngle ();
                case Char::b: return b_ ();
                case Char::c: return identifier ("c");
                case Char::d: return d_ ();
                case Char::e: return identifier ("e");
                case Char::f: return identifier ("f");
                case Char::g: return identifier ("g");
                case Char::i: return identifier ("i");
                case Char::n: return n_ ();
                case Char::o: return identifier ("o");
                case Char::p: return identifier ("p");
                case Char::r: return identifier ("r");
                case Char::s: return identifier ("s");
                case Char::t: return identifier ("t");
                case Char::u: return identifier ("u");
                case Char::v: return identifier ("v");
                case Char::w: return identifier ("w");
                case Char::BackSlash:
                    let c = escapeSequence ();
                    return identifier (String.fromCharCode(c));
                case Char::Zero: return zero ();
                case Char::One:
                case Char::Two:
                case Char::Three:
                case Char::Four:
                case Char::Five:
                case Char::Six:
                case Char::Seven:
                case Char::Eight:
                case Char::Nine:
                    return decimalInteger ();
                default:
                    if (Unicode.isIdentifierStart (String.fromCharCode(c)))
                    {
                        return identifier (String.fromCharCode(c));
                    }
                    else
                    {
                        return intrinsic::print("invalid prefix ", c);
                    }
                }
            }
            Debug.assert(false);
	}

	private function zero ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::x:
	    case Char::X:
		return hexLiteral ();
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
		return octalLiteral ();
	    case Char::Dot:
		return decimalInteger ();
	    case Char::Eight:  // what do we do with these?
	    case Char::Nine:
	    default :
		retract ();
		return numberSuffix ();
	    }
	}

	private function hexLiteral ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
	    case Char::Eight:
	    case Char::Nine:
	    case Char::a: case Char::A:
	    case Char::b: case Char::B:
	    case Char::c: case Char::C:
	    case Char::d: case Char::D:
	    case Char::e: case Char::E:
	    case Char::f: case Char::F:
		return hexLiteral ();
	    default:
		retract ();
		return numberSuffix ();
	    }
	}

	private function octalLiteral ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
		return octalLiteral ();
	    case Char::Eight:  // what do we do with these?
	    case Char::Nine:
	    default:
		retract ();
		return numberSuffix ();
	    }
	}

	private function decimalInteger ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
	    case Char::Eight:
	    case Char::Nine:
		return decimalInteger ();
	    case Char::Dot:
		return decimalFraction ();
	    case Char::e: case Char::E:
		return decimalExponent ();
	    default:
		retract ();
		return numberSuffix ();
	    }
	}

	private function decimalFraction ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
	    case Char::Eight:
	    case Char::Nine:
		return decimalFraction ();
	    case Char::e: case Char::E:
		switch (next()) {
		case Char::Plus:
		case Char::Minus:
		    return decimalExponent ();
		default:
		    retract ();
		    return decimalExponent ();
		}
	    default:
		retract ();
		return numberSuffix ();
	    }
	}

	private function decimalExponent ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
	    case Char::Eight:
	    case Char::Nine:
		return decimalExponent ();
	    default:
		retract ();
		return numberSuffix ();
	    }
	}

	private function numberSuffix ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::i:
		return Token::makeInstance (Token::ExplicitIntLiteral, lexeme ());
	    case Char::u:
		return Token::makeInstance (Token::ExplicitUIntLiteral, lexeme ());
	    case Char::d:
		return Token::makeInstance (Token::ExplicitDoubleLiteral, lexeme ());
	    case Char::m:
		return Token::makeInstance (Token::ExplicitDecimalLiteral, lexeme ());
	    default:
		retract ();
		return Token::makeInstance (Token::DecimalLiteral, lexeme ());
	    }
	}

	private function slash ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Slash:
		lineComment ();
		return start ();
	    case Char::Asterisk:
		blockComment ();
		return start ();
	    default:
		retract ();
		return Token::BREAK;
	    }
	}

	private function lineComment ()
	    : void
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Newline:
	    case Char::EOS:
		retract (); // leave newline for asi
		return;
	    default:
		return lineComment ();
	    }
	}

	private function blockComment ()
	    : void
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Asterisk :
            switch (next()) {
            case Char::Slash:
                return;
            case Char::EOS :
                retract ();
                return;
            case Char::Asterisk:
                retract (); // leave in case next char is a slash
                return blockComment ();
            case Char::Newline:
                colCoord = 0;
                lnCoord++; // count ln and fall through
            default:
                return blockComment ();
            }
	    case Char::EOS :
            retract ();
            return;
        case Char::Newline:
            lnCoord++; // fall through
	    default :
            return blockComment ();
	    }
	}

	private function stringLiteral (delimiter, text="")
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case delimiter:
            return Token::makeInstance (Token::StringLiteral, String.fromCharCode(delimiter)+text);
            // encode delimiter in string lexeme by appending to text
	    case Char::BackSlash:
            let c = escapeSequence ();
            return stringLiteral (delimiter, text+String.fromCharCode(c));
	    default:
            return stringLiteral (delimiter, text+String.fromCharCode (c))
	    }
	}

	/*

	*/

	private function escapeSequence ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
            retract ();
            return octalOrNulEscape ();
	    case Char::x:
            return hexEscape (2);
	    case Char::u:
            return hexEscape (4);
	    case Char::b:
            return Char::Backspace;
	    case Char::f:
            return Char::Formfeed;
	    case Char::n:
            return Char::Newline;
	    case Char::r:
            return Char::CarriageReturn;
	    case Char::t:
            return Char::Tab;
	    case Char::v:
            return Char::VerticalTab;
	    case Char::SingleQuote:
	    case Char::DoubleQuote:
	    case Char::BackSlash:
            return c;
        default:
            throw "lexer error escapeSequence " + c;
	    }
	}

	private function octalOrNulEscape (n:int)
	    : uint
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
		switch (next()) {
		case Char::One:
		case Char::Two:
		case Char::Three:
		case Char::Four:
		case Char::Five:
		case Char::Six:
		case Char::Seven:
		    retract ();
		    return octalEscapeFull (n+1);
		default:
		    return 0;  // \0
		}
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
		return octalEscapeFull (n+1);
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
		return octalEscapeShort (n+1);
	    default:
		throw "internal error: expecting octal character";
	    }
	}

	private function octalEscapeFull (n:int)
	    : uint
	{
	    if (n==3) {
		for (let i=0; i<n; i++ ) retract ();  // unwind input for rescanning
		return octalEscape (n);
	    }

	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
		return octalEscapeFull (n+1);
	    default:
		for (let i=0; i<=n; i++ ) retract ();  // unwind input for rescanning
		return octalEscape (n);
	    }
	}

	private function octalEscapeShort (n:int)
	    : uint
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
		for (let i=0; i<=n; i++ ) retract ();  // unwind input for rescanning
		return octalEscape (n+1);
	    default:
		for (let i=0; i<=n; i++ ) retract ();  // unwind input for rescanning
		return octalEscape (n);
	    }
	}

	private function octalEscape (n:int,v:uint=0)
	    : uint
	{
	    if (n==0) {
		return v;
	    }

	    let c : int = next ();
	    var m;
	    switch (c) {
	    case Char::Zero:
		m=0x0;
		break;
	    case Char::One:
		m=0x1;
		break;
	    case Char::Two:
		m=0x2;
		break;
	    case Char::Three:
		m=0x3;
		break;
	    case Char::Four:
		m=0x4;
		break;
	    case Char::Five:
		m=0x5;
		break;
	    case Char::Six:
		m=0x6;
		break;
	    case Char::Seven:
		m=0x7;
		break;
	    default:
		print("error");
		throw "malformed escape, expecting "+n+" more characters";
	    }
	    return octalEscape (n-1, v+m*Math.pow(8,n-1));
	}

	private function hexEscape (n:int,v:uint=0)
	    : uint
	{
	    if (n==0) {
		return v;
	    }

	    let c : int = next ();
	    var m;
	    switch (c) {
	    case Char::Zero:
		m=0x0;
		break;
	    case Char::One:
		m=0x1;
		break;
	    case Char::Two:
		m=0x2;
		break;
	    case Char::Three:
		m=0x3;
		break;
	    case Char::Four:
		m=0x4;
		break;
	    case Char::Five:
		m=0x5;
		break;
	    case Char::Six:
		m=0x6;
		break;
	    case Char::Seven:
		m=0x7;
		break;
	    case Char::Eight:
		m=0x8;
		break;
	    case Char::Nine:
		m=0x9;
		break;
	    case Char::a: case Char::A:
		m=0xA;
		break;
	    case Char::b: case Char::B:
		m=0xB;
		break;
	    case Char::c: case Char::C:
		m=0xC;
		break;
	    case Char::d: case Char::D:
		m=0xD;
		break;
	    case Char::e: case Char::E:
		m=0xE;
		break;
	    case Char::f: case Char::F:
		m=0xF
		break;
	    default:
		print("error");
		throw "malformed escape, expecting "+n+" more characters";
	    }
	    return hexEscape (n-1, v+m*Math.pow(16,n-1));
	}

	/*

	. .. ... .<

	*/

	private function dot ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Dot : return dotdot ();
	    case Char::LeftAngle : return Token::LeftDotAngle;
	    case Char::Zero:
	    case Char::One:
	    case Char::Two:
	    case Char::Three:
	    case Char::Four:
	    case Char::Five:
	    case Char::Six:
	    case Char::Seven:
	    case Char::Eight:
	    case Char::Nine:
		return decimalFraction ();
	    default :
		retract ();
		return Token::Dot;
	    }
	}

	private function dotdot ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Dot : return Token::TripleDot;
	    default :
		retract ();
		return Token::DoubleDot;
	    }
	}

	/*

	! != !==

	*/

	private function not ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return notequal ();
	    default :
		retract ();
		return Token::Not;
	    }
	}

	private function notequal ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::StrictNotEqual;
	    default :
		retract ();
		return Token::NotEqual;
	    }
	}

	/*

	% %=

	*/

	private function remainder ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::RemainderAssign;;
	    default :
		retract ();
		return Token::Remainder;
	    }
	}

	/*

	& &= && &&=

	*/

	private function and ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::BitwiseAndAssign;;
	    case Char::Ampersand : return logicalAnd ();
	    default :
		retract ();
		return Token::BitwiseAnd;
	    }
	}

	private function logicalAnd ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::LogicalAndAssign;;
	    default :
		retract ();
		return Token::LogicalAnd;
	    }
	}

	/*

	* *=

	*/

	private function mult ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::MultAssign;;
	    default :
		retract ();
		return Token::Mult;
	    }
	}

	/*

	+ +==

	*/

	private function plus ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Plus : return Token::PlusPlus;
	    case Char::Equal : return Token::PlusAssign;
	    default :
		retract ();
		return Token::Plus;
	    }
	}

	/*

	- -- -=

	*/

	private function minus ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Dash : return Token::MinusMinus;
	    case Char::Equal : return Token::MinusAssign;
	    default :
		retract ();
		return Token::Minus;
	    }
	}

	/*

	/ /= />

	*/

	public function div ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::DivAssign;
	    case Char::RightAngle : return Token::XmlTagEndEnd;
	    default :
		retract ();
		return Token::Div;
	    }
	}

	/*

	< <= </ << <<=

	*/

	private function leftAngle ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::LessThanOrEqual;
	    case Char::LeftAngle : return leftShift ();
	    case Char::Slash : return Token::XmlTagStartEnd
	    default :
		retract ();
		return Token::LessThan;
	    }
	}

	private function leftShift ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::LeftShiftAssign;
	    default :
		retract ();
		return Token::LeftShift;
	    }
	}

	/*

	= == ===

	*/

	private function equal ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return equalEqual ();
	    default :
		retract ();
		return Token::Assign;
	    }
	}

	private function equalEqual ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::StrictEqual;
	    default :
		retract ();
		return Token::Equal;
	    }
	}


	/*

	> >= >> >>= >>> >>>=

	*/

	private function rightAngle ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::GreaterThanOrEqual;
	    case Char::RightAngle : return rightShift ();
	    default :
		retract ();
		return Token::GreaterThan;
	    }
	}

	private function rightShift ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::RightShiftAssign;
	    case Char::RightAngle : return unsignedRightShift ();
	    default :
		retract ();
		return Token::RightShift;
	    }
	}

	private function unsignedRightShift ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::UnsignedRightShiftAssign;
	    default :
		retract ();
		return Token::UnsignedRightShift;
	    }
	}

	/*

	^ ^=

	*/

	private function bitwiseXor ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::BitwiseXorAssign;
	    default :
		retract ();
		return Token::BitwiseXor;
	    }
	}

	/*

	| |= || ||=

	*/

	private function bitwiseOr ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::BitwiseOrAssign;
	    case Char::Bar : return logicalOr ();
	    default :
		retract ();
		return Token::BitwiseOr;
	    }
	}

	private function logicalOr ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Equal : return Token::LogicalOrAssign;
	    default :
		retract ();
		return Token::LogicalOr;
	    }
	}

	/*

	: ::

	*/

	private function colon ()
	    : int
	{
	    let c : int = next ();
	    switch (c) {
	    case Char::Colon : return Token::DoubleColon;
	    default :
		retract ();
		return Token::Colon;
	    }
	}

	/*

	identifier

	*/

        private function identifier (str:string)
            : int
        {
            let c : int = next ();
            //print("c[",curIndex-1,"]=",String.fromCharCode(c))
            switch (c)
            {
            case Char::a :
            case Char::b :
            case Char::c :
            case Char::d :
            case Char::e :
            case Char::f :
            case Char::g :
            case Char::h :
            case Char::i :
            case Char::j :
            case Char::k :
            case Char::l :
            case Char::m :
            case Char::n :
            case Char::o :
            case Char::p :
            case Char::q :
            case Char::r :
            case Char::s :
            case Char::t :
            case Char::u :
            case Char::v :
            case Char::w :
            case Char::x :
            case Char::y :
            case Char::z :
            case Char::A :
            case Char::B :
            case Char::B :
            case Char::C :
            case Char::D :
            case Char::E :
            case Char::F :
            case Char::G :
            case Char::H :
            case Char::I :
            case Char::J :
            case Char::K :
            case Char::L :
            case Char::M :
            case Char::N :
            case Char::O :
            case Char::P :
            case Char::Q :
            case Char::R :
            case Char::S :
            case Char::T :
            case Char::U :
            case Char::V :
            case Char::W :
            case Char::X :
            case Char::Y :
            case Char::Z :
		return identifier (str+String.fromCharCode(c));
	    case Char::BackSlash:
		let c = escapeSequence ();
		return identifier (str+String.fromCharCode(c));
            default:
                if (Unicode.isIdentifierPart (String.fromCharCode(c)) && c != Char::EOS)
                {
                    return identifier (str+String.fromCharCode(c));
                }
                else
                {
                    retract ();
                    return Token::maybeReservedIdentifier (str);
                }
            }
        }

        private function b_ () : int
        {
            let c : int = next ();
            switch (c)
            {
                case Char::r:
		    return br_ ();
                default:
                    retract ();
                    return identifier ("b");
            }
        }

        private function br_ ()
            : int
        {
            let c : int = next ();
            switch (c)
            {
                case Char::e :
		    return identifier ("bre");
                default:
                    retract ();
                    return identifier ("br");
            }
        }

        private function d_ ()
            : int
        {
            let c : int = next ();
            switch (c)
            {
                case Char::e : return identifier ("de");
                default:
                    retract ();
                    return identifier ("d");
            }
        }

        private function n_ ()
            : int
        {
            let c : int = next();
            switch (c)
            {
                case Char::a : return identifier ("na");
                case Char::e : return identifier ("ne");
                case Char::u : return nu_ ();
                default:
                    retract ();
                    return identifier ("n");
            }
        }

        private function nu_ ()
            : int
        {
            let c : int = next ();
            switch (c) {
            case Char::l : return nul_ ();
            default:
                retract ();
                return identifier ("nu");
            }
        }

        private function nul_ ()
            : int
        {
            let c : int = next ();
            switch (c) {
            case Char::l : return null_ ();
            default:
                retract ();
                return identifier ("nul");
            }
        }

        private function null_ ()
            : int
        {
            let c : int = next ();
            if (Unicode.isIdentifierPart (String.fromCharCode(c)))
            {
                return identifier ("null"+String.fromCharCode(c));
            }
            else
            {
                retract();
                return Token::Null;
            }
        }
    }

    function test()
    {
        let testCases = [ "break case catch continue default delete do else enum extends"
			, "false finally for function if in instanceof new null return"
                        , "super switch this throw true try typeof var void while with"
			, "call cast const decimal double dynamic each eval final get has"
			, "implements import int interface internal intrinsic is let namespace"
			, "native Number override package precision private protected prototype public"
			, "rounding standard strict static to type uint undefined use xml yield"
			, ". .< .. ... ! != !== % %= & && &&= * *= + +- ++ - -- -="
		        , "/ /= /> < <= </ << <<= = == === > >= >> >>= >>> >>>="
			, "^ ^= | |= || ||= : :: ( ) [ ] { } ~ @ , ; ?"
			, "/* hello nobody */ hello // goodbye world"
			, "0 0i 00 001u 0123d 045m 0x0 0xCAFEBABE 0x12345678u 1. .0 .2e+3 1.23m"
			, "\\u0050 \\x50gh \\073 \\73 \\073123 \\7398"
                        , "/abc/ 'hi' \"bye\" null break /def/xyz" ].reverse();

        /*
	while (testCases.length > 0) {
            var scan = new Scanner (testCases.pop());
            var list = scan.tokenList (scan.start);
            let tk = Token::EOS;
            do {
                print("tokenList.length=",list.length);
                while (list.length > 0) {
                    tk=list.pop();
                    if (tk == Token::BREAK) {
			if (testCases.length == 0) {   // if last test, then scan for regexps
                            list = scan.tokenList (scan.regexp);
                        }
			else {
			    list = scan.tokenList (scan.div);
			}
                        print("tokenList.length=",list.length);
                    }
                }
            } while (tk != Token::EOS);
            print ("scanned!");
	}
        */
    }
    //    test ();
}
