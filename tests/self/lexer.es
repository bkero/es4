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

package Char
{
    use namespace intrinsic;

    public const Eof = 0;
    public const a = "a".charCodeAt(0);
    public const b = "b".charCodeAt(0);
    public const c = "c".charCodeAt(0);
    public const d = "d".charCodeAt(0);
    public const e = "e".charCodeAt(0);
    public const f = "f".charCodeAt(0);
    public const g = "g".charCodeAt(0);
    public const h = "h".charCodeAt(0);
    public const i = "i".charCodeAt(0);
    public const j = "j".charCodeAt(0);
    public const k = "k".charCodeAt(0);
    public const l = "l".charCodeAt(0);
    public const m = "m".charCodeAt(0);
    public const n = "n".charCodeAt(0);
    public const o = "o".charCodeAt(0);
    public const p = "p".charCodeAt(0);
    public const q = "q".charCodeAt(0);
    public const r = "r".charCodeAt(0);
    public const s = "s".charCodeAt(0);
    public const t = "t".charCodeAt(0);
    public const u = "u".charCodeAt(0);
    public const v = "v".charCodeAt(0);
    public const w = "w".charCodeAt(0);
    public const x = "x".charCodeAt(0);
    public const y = "y".charCodeAt(0);
    public const z = "z".charCodeAt(0);
    public const A = "A".charCodeAt(0);
    public const B = "B".charCodeAt(0);
    public const C = "C".charCodeAt(0);
    public const D = "D".charCodeAt(0);
    public const E = "E".charCodeAt(0);
    public const F = "F".charCodeAt(0);
    public const G = "G".charCodeAt(0);
    public const H = "H".charCodeAt(0);
    public const I = "I".charCodeAt(0);
    public const J = "J".charCodeAt(0);
    public const K = "K".charCodeAt(0);
    public const L = "L".charCodeAt(0);
    public const M = "M".charCodeAt(0);
    public const N = "N".charCodeAt(0);
    public const O = "O".charCodeAt(0);
    public const P = "P".charCodeAt(0);
    public const Q = "Q".charCodeAt(0);
    public const R = "R".charCodeAt(0);
    public const S = "S".charCodeAt(0);
    public const T = "T".charCodeAt(0);
    public const U = "U".charCodeAt(0);
    public const V = "V".charCodeAt(0);
    public const W = "W".charCodeAt(0);
    public const X = "X".charCodeAt(0);
    public const Y = "Y".charCodeAt(0);
    public const Z = "Z".charCodeAt(0);
    public const At = "@".charCodeAt(0);
    public const LeftParen = "(".charCodeAt(0);
    public const RightParen = ")".charCodeAt(0);
    public const Comma = ",".charCodeAt(0);
    public const Semicolon = ";".charCodeAt(0);
    public const Space = " ".charCodeAt(0);
    public const QuestionMark = "?".charCodeAt(0);
    public const LeftBracket = "[".charCodeAt(0);
    public const RightBracket = "]".charCodeAt(0);
    public const Tilde = "~".charCodeAt(0);
    public const Newline = "\n".charCodeAt(0);
    public const SingleQuote = "'".charCodeAt(0);
    public const DoubleQuote = "\"".charCodeAt(0);
    public const Dash = "-".charCodeAt(0);
    public const Bang = "!".charCodeAt(0);
    public const Percent = "%".charCodeAt(0);
    public const Ampersand = "&".charCodeAt(0);
    public const Mult = "*".charCodeAt(0);
    public const Slash = "/".charCodeAt(0);
    public const Colon = ":".charCodeAt(0);
    public const Caret = "^".charCodeAt(0);
    public const Bar = "|".charCodeAt(0);
    public const Plus = "+".charCodeAt(0);
    public const LeftAngle = "<".charCodeAt(0);
    public const RightAngle = ">".charCodeAt(0);
    public const LeftCurley = "{".charCodeAt(0);
    public const RightCurley = "}".charCodeAt(0);
    public const Equal = "=".charCodeAt(0);
    public const Zero = "0".charCodeAt(0);
    public const Nine = "9".charCodeAt(0);

    public function isDigit (ch:int) : Boolean {
        if( ch >= Zero && ch <= Nine ) return true
    }

    public function isIdentifierStart (ch:int) : Boolean {
        if( ch >= a && ch <= z ) return true
    }

    public function isIdentifierPart (ch:int) : Boolean {
        if( ch >= a && ch <= z ) return true
    }
}

package Lexer 
{
    use namespace intrinsic;
    import Char.*;
    import Token.*;

    public class Scanner 
    {
        private var tokenList : Array;
		private var followsNewline : Boolean;

        private var src : String;
        private var origin : String;
        private var curIndex : int;
        private var markIndex : int;

        public function Scanner (src:String, origin:String)
            : src = src
            , origin = origin
            , tokenStore = new Array
            , tokenList = new Array
            , curIndex = -1
            , markIndex = 0
        {
		}
    		
        public function nextchar()
        {
            print("curIndex=",curIndex+1);
            print("src.length=",src.length)
            // FIXME: translate unicode escapes here
            if (curIndex+1 < src.length)
            {
                var ch = src.charCodeAt (++curIndex);
                print ("ch=",ch);
            }
            else
            {
                var ch = Char.Eof;
                print ("ch=",ch);
            }
			return ch;
		}
		
		public function lexeme() : String 
        {
			return src.slice(mark,curIndex); // copies text since last mark
		}

        public function retract() : void
        {
            curIndex--;
        }

        private function mark () : void
        {
            markIndex = curIndex;
        }

        public function makeTokenList ()
        {
            let tokenList = new Array;
            while (true) {
                let token = nextToken ();
                print("token=",token);
                tokenList.push (token);
                if (token === Token.LexBreak ||
                    token === Token.Eof ||
                    token === Token.Error)
                {
                    break;
                }
            }
            print("tokenList.length=",tokenList.length);
            return tokenList;
        }

        public function nextToken (resetState:Boolean = false) : int 
        {
            let result = start();
                print("nexttoken=",result);
            return result;
        }

		private function start() : int
        {
            var c : int;
            while (true) 
            {
                c = nextchar();
                mark();
                switch (c) 
                {
                case 0xffffffef: return utf8sig ();
                case Char.Eof: print("tk=",Token.Eof); return Token.Eof;
                case Char.At: return Token.At;
                case Char.LeftParen: return Token.LeftParen;
                case Char.RightParen: return Token.RightParen;
                case Char.Comma: return Token.Comma;
                case Char.Semicolon: return Token.Semicolon;
                case Char.Space: return Token.Space;
                case Char.QuestionMark: return Token.QuestionMark;
                case Char.LeftBracket: return Token.LeftBracket;
                case Char.RightBracket: return Token.RightBracket;
                case Char.LeftCurley: return Token.LeftBrace;
                case Char.RightCurley: return Token.RightBrace;
                case Char.Tilde: return Token.BitwiseNot;
                case Char.Newline: return Token.Newline;
                case Char.SingleQuote: return stringLiteral (c);
                case Char.DoubleQuote: return stringLiteral (c);
                case Char.Dash: return minus ();
                case Char.Bang: return not ();
                case Char.Percent: return remainder ();
                case Char.Ampersand: return and ();
                case Char.Mult: return mult ();
                case Char.Slash: return slash ();
                case Char.Colon: return colon ();
                case Char.Caret: return caret ();
                case Char.Bar: return or ();
                case Char.Plus: return plus ();
                case Char.LeftAngle: return lessthan ();
                case Char.Equal: return equal ();
                case Char.RightAngle: return greaterthan ();
                case Char.Zero: return zero ();
                case Char.b: return b ();
                case Char.c: return c ();
                case Char.d: return d ();
                case Char.e: return e ();
                case Char.f: return f ();
                case Char.g: return g ();
                case Char.i: return i ();
                case Char.n: return n ();
                case Char.o: return o ();
                case Char.p: return p ();
                case Char.r: return r ();
                case Char.s: return s ();
                case Char.t: return t ();
                case Char.u: return u ();
                case Char.v: return v ();
                case Char.w: return w ();
                default:
                    if (isDigit (c)) 
                    {
                        return digit (c);
                    } 
                    else if (isIdentifierStart (c)) 
                    {
                        return identifier (c);
                    } 
                    else 
                    {
                        return error ();
                    }
                }
            }
            Debug.assert(false);
		}

        private function identifier () : int 
        {
            print(">>identifier");
            let c : int = nextchar ();
            switch (c) 
            {
            case Char.a : identifier ();
            case Char.b : identifier ();
            case Char.c : identifier ();
            case Char.d : identifier ();
            default:
                if (isIdentifierPart (c))
                {
                    return identifier ();
                }
                else
                {
                    retract ();                    
                    return Token.reservedWordOrIdentifier (lexeme ());
                }
            }
        }

        private function n_ () : int 
        {
            let c : int = nextchar();
            switch (c) 
            {
                case Char.a : return na_ ();
                case Char.e : return ne_ ();
                case Char.u : return nu_ ();
                default:
                    retract ();
                    return identifier ();
            }
        }

        private function nu_ () : int 
        {
            let c : int = nextchar ();
            switch (c) 
            {
                case Char.l : nul_ ();
                default:
                    retract ();
                    return identifier ();
            }
        }

        private function nul_ () : int 
        {
            let c : int = nextchar ();
            switch (c) 
            {
            case Char.l : null_ ();
            default:
                retract ();
                return identifier ();
            }
        }

        private function null_ () : int 
        {
            let c : int = nextchar ();
            if (isIdentifierPart (c))
            {
                return identifier ();
            }
            else
            {
                return Token.Null;
            }
        }
    }

    public function test () 
    {
        var scanner = new Scanner ("a","test1");
        var tokenList = scanner.makeTokenList ();
        print (tokenList);
    }
}

Lexer.test()
