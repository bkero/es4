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
}

package Lexer 
{
    public class Scanner 
    {
        private var tokenStore : Array;
        private var tokenList : Array;
		private var followsNewline : Boolean;

        private var src : String;
        private var curIndex : int;

        public function Scanner (src:String, origin:String)
            : src = src
            , origin = origin
            , tokenStore = new Array
            , tokenList = new Array
            , curIndex = 0
        {
		}
    		
        public function nextchar() : int 
        {
            // FIXME: translate unicode escapes here
			return src.charCodeAt (++cur)
		}
		
		public function lexeme() : String 
        {
			return input.copy(); // copies text since last mark
		}

        public function retract() : void
        {
            curIndex--;
        }

        public function makeInstance(token_class:int, lexeme:String) : int
        {
            tokenStore.push(new Token(token_class, lexeme));
            return tokenStore.length - 1;
        }

        public function getTokenClass (token_id : int) : int
        {
            // if the token id is negative, it is a token_class

            if (token_id < 0)
            {
               return token_id;
            }

            // otherwise, get instance data from the instance vector.

            var t : Token = tokens[token_id];
            return t.getTokenClass();
        }
        
        public function getTokenText ( token_id : int ) : String
        {
            // if the token id is negative, it is a token_class.

            if (token_id < 0)
            {
                return Token.getTokenClassName(token_id);
            }

            // otherwise, get instance data from the instance vector.

            var t : Token = tokens[token_id];
            return t.getTokenText();
        }

        public function nexttoken(resetState:Boolean = false) : int 
        {
            let result = start()
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
                case 0: return Token.Eos;
                case Char.At: return Token.At;
                case Char.LeftParen: return Token.LeftParen;
                case Char.RightParen: return Token.RightParen;
                case Char.Comma: return Token.Comma;
                case Char.Semicolon: return Token.Semicolon;
                case Char.Space: return Token.Space;
                case Char.QuestionMark: return Token.QuestionMark;
                case Char.LeftBracket: return Token.LeftBracket;
                case Char.RightBracket: return Token.RightBracket;
                case Char.LeftBrace: return Token.LeftBrace;
                case Char.RightBrace: return Token.RightBrace;
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
                case Char.a: return a ();
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
                    return Token.maybeReservedIdentifierToken (lexeme ());
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
}
