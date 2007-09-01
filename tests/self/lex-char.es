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

public namespace Char

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
    const UnderScore = "_".charCodeAt(0);
    const Dollar = "$".charCodeAt(0);
    const Space = " ".charCodeAt(0);
    const Tab = "\t".charCodeAt(0);
    const VerticalTab = "\v".charCodeAt(0);
    const Newline = "\n".charCodeAt(0);
    const CarriageReturn = "\r".charCodeAt(0);

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

    function isIdentifierStart(c) {
        if (c >= Char::A && c <= Char::Z) return true;
        else if (c >= Char::a && c <= Char::z) return true;
        else if (c == Char::UnderScore) return true;
        else if (c == Char::Dollar) return true;
        return false;
    }

    function isDigit (c) {
        if (c >= Char::Zero && c <= Char::Nine) return true;
        return false;
    }

    function isIdentifierPart(c) {
        if (isIdentifierStart (c)) return true;
        else if (isDigit (c)) return true;
        return false;
    }


    function test () {
	print ("testing lex-char.es");
        print ("Space=",Space);
        print ("Tab=",Tab);
        print ("Newline=",Newline);
    }

    Char::test ();

}
