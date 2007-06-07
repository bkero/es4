/* Unicode tables.
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

package Unicode
{
    use strict;
    use namespace intrinsic;

    function makeTable(data) {
        var t = [];
        t.watermark = -1;
        t.data = data;
        t.ptr = 0;
        return t;
    }

    function expandTable(tbl, cc:uint) {
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

    function classify(tbl, cc:uint): boolean {
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

}
