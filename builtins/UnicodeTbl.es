/* -*- mode: java; indent-tabs-mode: nil -*-
 *
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
 *
 * Unicode tables.
 *
 * Classification tables:
 *
 * In order to reduce boot times of the reference implementation the
 * unicode classification tables are coded as strings that are
 * expanded into tables lazily.  The strings are generated from
 * Unicode character files and are found in UnicodeClasses.es in this
 * directory.  That file must be loaded before this one.
 *
 * The strings for classification tables are semi-compactly coded
 * using character ranges when appropriate: \x{mmmm}--\x{nnnn} in the
 * string denotes the range [0xmmmm,0xnnnn].
 *
 *
 * Case mapping tables:
 *
 * ...
 *
 * The raw unicode character data and the scripts to process them are
 * in the directory ../unicode.
 */
    use namespace ECMAScript4_Internal;
    use namespace intrinsic;
    use namespace Unicode;
    use default namespace Unicode;

    function makeTable(data) {
        var t = [];
        t.watermark = -1;
        t.data = data;
        t.ptr = 0;
        return t;
    }

    function expandTable(tbl, cc:double) {
        while (tbl.watermark < cc) {
            if (tbl.ptr == tbl.data.length)
                tbl.watermark = uint.MAX_VALUE;
            else {
                let c = tbl.data.charCodeAt(tbl.ptr++);
                // 0x2D is '-', we're looking for '--'
                if (tbl.ptr < tbl.data.length-3 && tbl.data.charCodeAt(tbl.ptr) == 0x2D && tbl.data.charCodeAt(tbl.ptr+1) == 0x2D) {
                    let d = tbl.data.charCodeAt(tbl.ptr+2);
                    tbl.ptr += 3;
                    for ( let i=c ; i <= d ; i++ )
                        tbl[i] = true;
                    tbl.watermark = d;
                }
                else {
                    tbl[c] = true;
                    tbl.watermark = c;
                }
            }
        }
    }

    var table_Lu = makeTable(category_Lu);
    var table_Ll = makeTable(category_Ll);
    var table_Lt = makeTable(category_Lt);
    var table_Lm = makeTable(category_Lm);
    var table_Lo = makeTable(category_Lo);
    var table_Mn = makeTable(category_Mn);
    var table_Mc = makeTable(category_Mc);
    var table_Me = makeTable(category_Me);
    var table_Nd = makeTable(category_Nd);
    var table_Nl = makeTable(category_Nl);
    var table_No = makeTable(category_No);
    var table_Pc = makeTable(category_Pc);
    var table_Pd = makeTable(category_Pd);
    var table_Ps = makeTable(category_Ps);
    var table_Pe = makeTable(category_Pe);
    var table_Pi = makeTable(category_Pi);
    var table_Pf = makeTable(category_Pf);
    var table_Po = makeTable(category_Po);
    var table_Sm = makeTable(category_Sm);
    var table_Sc = makeTable(category_Sc);
    var table_Sk = makeTable(category_Sk);
    var table_So = makeTable(category_So);
    var table_Zs = makeTable(category_Zs);
    var table_Zl = makeTable(category_Zl);
    var table_Zp = makeTable(category_Zp);
    var table_Cc = makeTable(category_Cc);
    var table_Cf = makeTable(category_Cf);
    var table_Cs = makeTable(category_Cs);
    var table_Co = makeTable(category_Co);
    var table_Cn = makeTable(category_Cn);

    function classify(tbl, cc:double): boolean {
        expandTable(tbl, cc);
        return tbl.hasOwnProperty(cc) ? true : false;
    }


    function toUpperTbl(i) {
        initUpperTbl();
        return upperTbl[i];
    }

    function toLowerTbl(i) {
        initLowerTbl();
        return lowerTbl[i];
    }
