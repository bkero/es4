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
 */

    use namespace intrinsic;

    // JSON formatting with cycle checking.

    public function formatObject(object:Object, pretty=false): string {
        pp = pretty;
        return fmtObject(object);
    }

    public function formatArray(array:Array, pretty=false): string {
        pp = pretty;
        return fmtArray(array);
    }

    public function formatDate(date:Date, pretty=false): string
        '"' + date.intrinsic::toISOString() + '"';

    public function formatString(s:string, pretty=false): string {
        // It's implementation-dependent whether eg BACKSLASH is
        // serialized as "\\" or "\u005C".  That implementation
        // dependency is not expressed here, but it could be expressed
        // by factoring out formatting for BACKSLASH QUOTATION
        // BACKSPACE FORMFEED NEWLINE RETURN TAB as an informative
        // function.
        //
        // In the same vein it's actually implementation-dependent
        // whether the letter "A" is serialized as "A" or as "\u0040",
        // so in principle we should factor that too?
        //
        // FIXME: Must deal with unicode outside BMP by splitting
        // as a surrogate pair.
        let s = '"';
        for ( let i=0, limit=s.length ; i < limit ; i++ ) {
            let c = s.charCodeAt(i);
            if (c < 0x20 || c == 0x22 || c == 0x5C) {
                let t = c.toString(16);
                if (t.length == 1)
                    t = '0' + t;
                s += "\\u00" + t;
            }
            else
                s += c.charAt(i);
        }
        s += '"';
        return s;
    }

    public function formatNumber(n:AnyNumber, pretty=false): string {
        if (isNaN(n) || !isFinite(n))
            return "null";
        return string(n);
    }

    public function formatBoolean(bool:boolean, pretty=false): string
        bool ? "true" : "false";


    var pp = false;    // true for pretty-printing
    var level = 0;     // indentation level
    var active = new ObjectMap;

    // The only way to ask if something is of a specific type (and not
    // one of the subtypes) is through the use of the meta-objects
    // system, and then only if you're able to create at least one
    // dummy instance of the type from which to obtain a Type object.

    var objectType = intrinsic::typeOf({});  // Type object for Object

    function isObject(v)
        objectType.isSubtypeOf(intrinsic::typeOf(v));

    function isEncodableValue(v) {
        return ((v is AnyString) ||
                (v is AnyBoolean) ||
                (v is AnyNumber) ||
                (v is null) ||
                (v is Date) ||
                (v is Array) ||
                isObject(v));
    }

    function isEncodableName(n) {
        return (n is double || 
                n is string || 
                n is Name && n.qualifier == null);
    }

    function fmtObject(obj)
        indent + "{" + newline + fmtFields(obj, level+1) + newline + indent + "}";

    function fmtFields(obj, newlevel) {
        let oldlevel = level;
        try {
            let s = "";
            level = newlevel;
            active.activate(obj);
            let first = true;
            for ( let prop in obj ) {
                if (isEncodableName(prop) && obj.hasOwnProperty(prop)) {
                    let value = obj[prop];
                    if (isEncodableValue(value)) {
                        if (!first)
                            s += comma;
                        first = false;
                        s += (indent + "\"" + prop + "\"" + colon + 
                              value.public::toJSONString(pp));
                    }
                }
            }
            return s;
        }
        finally {
            level = oldlevel;
            active.deactivate(obj);
        }
    }

    function fmtArray(obj)
        indent + "[" + newline + fmtElements(obj, level+1) + newline + indent + "]";

    function fmtElements(obj, newlevel) {
        let oldlevel = level;
        try {
            let s = "";
            let first = true;
            level = newlevel;
            active.activate(obj);
            for ( let i=0, limit=Number(obj.length) ; i < limit ; i++ ) {
                let value = obj[i];
                if (isEncodableValue(value)) {
                    if (!first)
                        s += comma;
                    first = false;
                    s += (indent + value.public::toJSONString(pp));
                }
            }
        }
        finally {
            level = oldlevel;
            active.deactivate(obj);
        }
    }

    function get colon() pp ? ": " : ":";
    function get comma() pp ? ",\n" : ",";
    function get newline() pp ? "\n" : "";
    function get indent() pp ? (new Array(level*4 + 1)).join(" ") : "";

    // This could be a Map.<*,null>, say
    class ObjectMap
    {
        var active = [];   // list of active objects (circularity detection) (could be a Map)

        function activate(obj) {
            if (Array.indexOf(active, obj) != -1)
                throw new EncodingError("Circular structure can't be JSON encoded");
            active.push(obj);
        }

        function deactivate(obj) {
            let probe = Array.indexOf(active, obj);
            if (probe == -1)
                throw new Error("Internal error: object not in active list!");
            active.slice(probe, 1);
        }
    }


    // JSON parsing with full error checking.

    // FIXME: implement filter
    public function parse(s: string, filter=undefined) {
        let len = s.length;
        let i = 0;

        let white = " \t\n\r";
        let punct = "{},:[]";

        function ws() {
            while (i < len && white.indexOf(s[i]) != -1)
                i++;
        }

        function stop(c)
            white.indexOf(s[i]) != -1 || punct.indexOf(s[i]) != -1;

        function eat(c) {
            ws();
            if (i < len && s[i] == c) {
                ++i;
                ws();
                return true;
            }
            return false;
        }

        function match(c) {
            if (!eat(c))
                throw new EncodingError();
        }

        function parseObject() {
            // Initial { has been consumed
            let obj = {};
            if (eat('}'))
                return obj;
            while (true) {
                let x = parseValue();
                if (!(x is string))
                    throw new EncodingError();
                match(':');
                let v = parseValue();
                obj[x] = v;
                if (eat('}'))
                    return obj;
                match(',');
            }
        }

        function parseArray() {
            // Initial [ has been consumed
            let array = [];
            let idx = 0;
            if (eat(']'))
                return array;
            while (true) {
                array[idx++] = parseValue();
                if (eat(']'))
                    return array;
                match(',');
            }
        }

        function parseString() {
            // Initial quote has been consumed
            let str = "";
            while (true) {
                if (i == len)
                    throw new EncodingError();
                if (i == '"')
                    return str;
                if (s[i] == '\\') {
                    i++;
                    if (i == len)
                        throw new EncodingError();
                    switch (s[i]) {
                    case '\\': str += '\\'; break;
                    case '"':  str += '"'; break;
                    case '/':  str += '/'; break;
                    case 'b':  str += '\x08'; break;
                    case 'f':  str += '\x0C'; break;
                    case 'n':  str += '\x0A'; break;
                    case 'r':  str += '\x0D'; break;
                    case 't':  str += '\x09'; break;
                    case 'u': {
                        ++i;
                        if (len - i < 4)
                            throw new EncodingError();
                        let v = s.substring(i,i+4);
                        i += 4;
                        if (/[A-Fa-f0-9]{4}/.exec(v) == null)
                            throw new EncodingError();
                        let chr = parseInt(v, 16);
                        // If chr is the first half of a surrogate pair, then look
                        // for the second one.  If found, and on a 21-bit Unicode system,
                        // then join them as one character; otherwise place chr and perhaps
                        // the next one individually into the string.
                        // FIXME.
                        str += chr;
                    }
                    default:
                        throw new EncodingError();
                    }
                }
                else
                    str += s.charAt(i++);
            }
        }

        function parseValue() {
            if (eat('{'))
                return parseObject();
            if (eat('['))
                return parseArray();
            if (i < len && s[i] == '"') {
                i++;
                return parseString();
            }

            let start = i-1;
            while (i < len && !stop(s[i]))
                i++;
            let token = s.substring(start, i);
            if (token === "true")
                return true;
            if (token === "false")
                return false;
            if (token === "null")
                return null;
            if (/^-?[0-9]+(?:\.[0-9]+)(?:[Ee][+-]?[0-9]+)$/.exec(token))
                return parseFloat(token);
            throw new EncodingError();
        }

        if (eat('{'))
            return parseObject();
        if (eat('['))
            return parseArray();

        throw new EncodingError();
    }
